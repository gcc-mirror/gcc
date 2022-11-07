------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Butil;  use Butil;
with Debug;  use Debug;
with Fname;  use Fname;
with Opt;    use Opt;
with Osint;  use Osint;
with Output; use Output;
with Snames; use Snames;

with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;
with System.String_Hash;

package body ALI is

   use ASCII;
   --  Make control characters visible

   -----------
   -- Types --
   -----------

   --  The following type represents an invocation construct

   type Invocation_Construct_Record is record
      Body_Placement : Declaration_Placement_Kind := No_Declaration_Placement;
      --  The location of the invocation construct's body with respect to the
      --  unit where it is declared.

      Kind : Invocation_Construct_Kind := Regular_Construct;
      --  The nature of the invocation construct

      Signature : Invocation_Signature_Id := No_Invocation_Signature;
      --  The invocation signature that uniquely identifies the invocation
      --  construct in the ALI space.

      Spec_Placement : Declaration_Placement_Kind := No_Declaration_Placement;
      --  The location of the invocation construct's spec with respect to the
      --  unit where it is declared.
   end record;

   --  The following type represents an invocation relation. It associates an
   --  invoker that activates/calls/instantiates with a target.

   type Invocation_Relation_Record is record
      Extra : Name_Id := No_Name;
      --  The name of an additional entity used in error diagnostics

      Invoker : Invocation_Signature_Id := No_Invocation_Signature;
      --  The invocation signature that uniquely identifies the invoker within
      --  the ALI space.

      Kind : Invocation_Kind := No_Invocation;
      --  The nature of the invocation

      Target : Invocation_Signature_Id := No_Invocation_Signature;
      --  The invocation signature that uniquely identifies the target within
      --  the ALI space.
   end record;

   --  The following type represents an invocation signature. Its purpose is
   --  to uniquely identify an invocation construct within the ALI space. The
   --  signature comprises several pieces, some of which are used in error
   --  diagnostics by the binder. Identification issues are resolved as
   --  follows:
   --
   --    * The Column, Line, and Locations attributes together differentiate
   --      between homonyms. In most cases, the Column and Line are sufficient
   --      except when generic instantiations are involved. Together, the three
   --      attributes offer a sequence of column-line pairs that eventually
   --      reflect the location within the generic template.
   --
   --    * The Name attribute differentiates between invocation constructs at
   --      the scope level. Since it is illegal for two entities with the same
   --      name to coexist in the same scope, the Name attribute is sufficient
   --      to distinguish them. Overloaded entities are already handled by the
   --      Column, Line, and Locations attributes.
   --
   --    * The Scope attribute differentiates between invocation constructs at
   --      various levels of nesting.

   type Invocation_Signature_Record is record
      Column : Nat := 0;
      --  The column number where the invocation construct is declared

      Line : Nat := 0;
      --  The line number where the invocation construct is declared

      Locations : Name_Id := No_Name;
      --  Sequence of column and line numbers within nested instantiations

      Name : Name_Id := No_Name;
      --  The name of the invocation construct

      Scope : Name_Id := No_Name;
      --  The qualified name of the scope where the invocation construct is
      --  declared.
   end record;

   ---------------------
   -- Data structures --
   ---------------------

   package Invocation_Constructs is new Table.Table
     (Table_Index_Type     => Invocation_Construct_Id,
      Table_Component_Type => Invocation_Construct_Record,
      Table_Low_Bound      => First_Invocation_Construct,
      Table_Initial        => 2500,
      Table_Increment      => 200,
      Table_Name           => "Invocation_Constructs");

   package Invocation_Relations is new Table.Table
     (Table_Index_Type     => Invocation_Relation_Id,
      Table_Component_Type => Invocation_Relation_Record,
      Table_Low_Bound      => First_Invocation_Relation,
      Table_Initial        => 2500,
      Table_Increment      => 200,
      Table_Name           => "Invocation_Relation");

   package Invocation_Signatures is new Table.Table
     (Table_Index_Type     => Invocation_Signature_Id,
      Table_Component_Type => Invocation_Signature_Record,
      Table_Low_Bound      => First_Invocation_Signature,
      Table_Initial        => 2500,
      Table_Increment      => 200,
      Table_Name           => "Invocation_Signatures");

   procedure Destroy (IS_Id : in out Invocation_Signature_Id);
   --  Destroy an invocation signature with id IS_Id

   function Hash
     (IS_Rec : Invocation_Signature_Record) return Bucket_Range_Type;
   --  Obtain the hash of key IS_Rec

   package Sig_Map is new Dynamic_Hash_Tables
     (Key_Type              => Invocation_Signature_Record,
      Value_Type            => Invocation_Signature_Id,
      No_Value              => No_Invocation_Signature,
      Expansion_Threshold   => 1.5,
      Expansion_Factor      => 2,
      Compression_Threshold => 0.3,
      Compression_Factor    => 2,
      "="                   => "=",
      Destroy_Value         => Destroy,
      Hash                  => Hash);

   --  The following map relates invocation signature records to invocation
   --  signature ids.

   Sig_To_Sig_Map : constant Sig_Map.Dynamic_Hash_Table :=
                      Sig_Map.Create (500);

   --  The folowing table maps declaration placement kinds to character codes
   --  for invocation construct encoding in ALI files.

   Declaration_Placement_Codes :
     constant array (Declaration_Placement_Kind) of Character :=
       (In_Body                  => 'b',
        In_Spec                  => 's',
        No_Declaration_Placement => 'Z');

   Compile_Time_Invocation_Graph_Encoding : Invocation_Graph_Encoding_Kind :=
                                              No_Encoding;
   --  The invocation-graph encoding format as specified at compile time. Do
   --  not manipulate this value directly.

   --  The following table maps invocation kinds to character codes for
   --  invocation relation encoding in ALI files.

   Invocation_Codes :
     constant array (Invocation_Kind) of Character :=
       (Accept_Alternative                     => 'a',
        Access_Taken                           => 'b',
        Call                                   => 'c',
        Controlled_Adjustment                  => 'd',
        Controlled_Finalization                => 'e',
        Controlled_Initialization              => 'f',
        Default_Initial_Condition_Verification => 'g',
        Initial_Condition_Verification         => 'h',
        Instantiation                          => 'i',
        Internal_Controlled_Adjustment         => 'j',
        Internal_Controlled_Finalization       => 'k',
        Internal_Controlled_Initialization     => 'l',
        Invariant_Verification                 => 'm',
        Postcondition_Verification             => 'n',
        Protected_Entry_Call                   => 'o',
        Protected_Subprogram_Call              => 'p',
        Task_Activation                        => 'q',
        Task_Entry_Call                        => 'r',
        Type_Initialization                    => 's',
        No_Invocation                          => 'Z');

   --  The following table maps invocation construct kinds to character codes
   --  for invocation construct encoding in ALI files.

   Invocation_Construct_Codes :
     constant array (Invocation_Construct_Kind) of Character :=
       (Elaborate_Body_Procedure => 'b',
        Elaborate_Spec_Procedure => 's',
        Regular_Construct        => 'Z');

   --  The following table maps invocation-graph encoding kinds to character
   --  codes for invocation-graph encoding in ALI files.

   Invocation_Graph_Encoding_Codes :
     constant array (Invocation_Graph_Encoding_Kind) of Character :=
       (Full_Path_Encoding => 'f',
        Endpoints_Encoding => 'e',
        No_Encoding        => 'Z');

   --  The following table maps invocation-graph line kinds to character codes
   --  used in ALI files.

   Invocation_Graph_Line_Codes :
     constant array (Invocation_Graph_Line_Kind) of Character :=
       (Invocation_Construct_Line        => 'c',
        Invocation_Graph_Attributes_Line => 'a',
        Invocation_Relation_Line         => 'r');

   --  The following variable records which characters currently are used as
   --  line type markers in the ALI file. This is used in Scan_ALI to detect
   --  (or skip) invalid lines.

   Known_ALI_Lines : constant array (Character range 'A' .. 'Z') of Boolean :=
     ('A' | --  argument
      'C' | --  SCO information
      'D' | --  dependency
      'E' | --  external
      'G' | --  invocation graph
      'I' | --  interrupt
      'K' | --  CUDA kernels
      'L' | --  linker option
      'M' | --  main program
      'N' | --  notes
      'P' | --  program
      'R' | --  restriction
      'S' | --  specific dispatching
      'T' | --  task stack information
      'U' | --  unit
      'V' | --  version
      'W' | --  with
      'X' | --  xref
      'Y' | --  limited_with
      'Z'   --  implicit with from instantiation
          => True,

      --  Still available:

      'B' | 'F' | 'H' | 'J' | 'O' | 'Q' => False);

   ------------------------------
   -- Add_Invocation_Construct --
   ------------------------------

   procedure Add_Invocation_Construct
     (Body_Placement : Declaration_Placement_Kind;
      Kind           : Invocation_Construct_Kind;
      Signature      : Invocation_Signature_Id;
      Spec_Placement : Declaration_Placement_Kind;
      Update_Units   : Boolean := True)
   is
   begin
      pragma Assert (Present (Signature));

      --  Create a invocation construct from the scanned attributes

      Invocation_Constructs.Append
        ((Body_Placement => Body_Placement,
          Kind           => Kind,
          Signature      => Signature,
          Spec_Placement => Spec_Placement));

      --  Update the invocation construct counter of the current unit only when
      --  requested by the caller.

      if Update_Units then
         declare
            Curr_Unit : Unit_Record renames Units.Table (Units.Last);

         begin
            Curr_Unit.Last_Invocation_Construct := Invocation_Constructs.Last;
         end;
      end if;
   end Add_Invocation_Construct;

   -----------------------------
   -- Add_Invocation_Relation --
   -----------------------------

   procedure Add_Invocation_Relation
     (Extra        : Name_Id;
      Invoker      : Invocation_Signature_Id;
      Kind         : Invocation_Kind;
      Target       : Invocation_Signature_Id;
      Update_Units : Boolean := True)
   is
   begin
      pragma Assert (Present (Invoker));
      pragma Assert (Kind /= No_Invocation);
      pragma Assert (Present (Target));

      --  Create an invocation relation from the scanned attributes

      Invocation_Relations.Append
        ((Extra   => Extra,
          Invoker => Invoker,
          Kind    => Kind,
          Target  => Target));

      --  Update the invocation relation counter of the current unit only when
      --  requested by the caller.

      if Update_Units then
         declare
            Curr_Unit : Unit_Record renames Units.Table (Units.Last);

         begin
            Curr_Unit.Last_Invocation_Relation := Invocation_Relations.Last;
         end;
      end if;
   end Add_Invocation_Relation;

   --------------------
   -- Body_Placement --
   --------------------

   function Body_Placement
     (IC_Id : Invocation_Construct_Id) return Declaration_Placement_Kind
   is
   begin
      pragma Assert (Present (IC_Id));
      return Invocation_Constructs.Table (IC_Id).Body_Placement;
   end Body_Placement;

   ----------------------------------------
   -- Code_To_Declaration_Placement_Kind --
   ----------------------------------------

   function Code_To_Declaration_Placement_Kind
     (Code : Character) return Declaration_Placement_Kind
   is
   begin
      --  Determine which placement kind corresponds to the character code by
      --  traversing the contents of the mapping table.

      for Kind in Declaration_Placement_Kind loop
         if Declaration_Placement_Codes (Kind) = Code then
            return Kind;
         end if;
      end loop;

      raise Program_Error;
   end Code_To_Declaration_Placement_Kind;

   ---------------------------------------
   -- Code_To_Invocation_Construct_Kind --
   ---------------------------------------

   function Code_To_Invocation_Construct_Kind
     (Code : Character) return Invocation_Construct_Kind
   is
   begin
      --  Determine which invocation construct kind matches the character code
      --  by traversing the contents of the mapping table.

      for Kind in Invocation_Construct_Kind loop
         if Invocation_Construct_Codes (Kind) = Code then
            return Kind;
         end if;
      end loop;

      raise Program_Error;
   end Code_To_Invocation_Construct_Kind;

   --------------------------------------------
   -- Code_To_Invocation_Graph_Encoding_Kind --
   --------------------------------------------

   function Code_To_Invocation_Graph_Encoding_Kind
     (Code : Character) return Invocation_Graph_Encoding_Kind
   is
   begin
      --  Determine which invocation-graph encoding kind matches the character
      --  code by traversing the contents of the mapping table.

      for Kind in Invocation_Graph_Encoding_Kind loop
         if Invocation_Graph_Encoding_Codes (Kind) = Code then
            return Kind;
         end if;
      end loop;

      raise Program_Error;
   end Code_To_Invocation_Graph_Encoding_Kind;

   -----------------------------
   -- Code_To_Invocation_Kind --
   -----------------------------

   function Code_To_Invocation_Kind
     (Code : Character) return Invocation_Kind
   is
   begin
      --  Determine which invocation kind corresponds to the character code by
      --  traversing the contents of the mapping table.

      for Kind in Invocation_Kind loop
         if Invocation_Codes (Kind) = Code then
            return Kind;
         end if;
      end loop;

      raise Program_Error;
   end Code_To_Invocation_Kind;

   ----------------------------------------
   -- Code_To_Invocation_Graph_Line_Kind --
   ----------------------------------------

   function Code_To_Invocation_Graph_Line_Kind
     (Code : Character) return Invocation_Graph_Line_Kind
   is
   begin
      --  Determine which invocation-graph line kind matches the character
      --  code by traversing the contents of the mapping table.

      for Kind in Invocation_Graph_Line_Kind loop
         if Invocation_Graph_Line_Codes (Kind) = Code then
            return Kind;
         end if;
      end loop;

      raise Program_Error;
   end Code_To_Invocation_Graph_Line_Kind;

   ------------
   -- Column --
   ------------

   function Column (IS_Id : Invocation_Signature_Id) return Nat is
   begin
      pragma Assert (Present (IS_Id));
      return Invocation_Signatures.Table (IS_Id).Column;
   end Column;

   ----------------------------------------
   -- Declaration_Placement_Kind_To_Code --
   ----------------------------------------

   function Declaration_Placement_Kind_To_Code
     (Kind : Declaration_Placement_Kind) return Character
   is
   begin
      return Declaration_Placement_Codes (Kind);
   end Declaration_Placement_Kind_To_Code;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (IS_Id : in out Invocation_Signature_Id) is
      pragma Unreferenced (IS_Id);
   begin
      null;
   end Destroy;

   -----------
   -- Extra --
   -----------

   function Extra (IR_Id : Invocation_Relation_Id) return Name_Id is
   begin
      pragma Assert (Present (IR_Id));
      return Invocation_Relations.Table (IR_Id).Extra;
   end Extra;

   -----------------------------------
   -- For_Each_Invocation_Construct --
   -----------------------------------

   procedure For_Each_Invocation_Construct
     (Processor : Invocation_Construct_Processor_Ptr)
   is
   begin
      pragma Assert (Processor /= null);

      for IC_Id in Invocation_Constructs.First ..
                   Invocation_Constructs.Last
      loop
         Processor.all (IC_Id);
      end loop;
   end For_Each_Invocation_Construct;

   -----------------------------------
   -- For_Each_Invocation_Construct --
   -----------------------------------

   procedure For_Each_Invocation_Construct
     (U_Id      : Unit_Id;
      Processor : Invocation_Construct_Processor_Ptr)
   is
      pragma Assert (Present (U_Id));
      pragma Assert (Processor /= null);

      U_Rec : Unit_Record renames Units.Table (U_Id);

   begin
      for IC_Id in U_Rec.First_Invocation_Construct ..
                   U_Rec.Last_Invocation_Construct
      loop
         Processor.all (IC_Id);
      end loop;
   end For_Each_Invocation_Construct;

   ----------------------------------
   -- For_Each_Invocation_Relation --
   ----------------------------------

   procedure For_Each_Invocation_Relation
     (Processor : Invocation_Relation_Processor_Ptr)
   is
   begin
      pragma Assert (Processor /= null);

      for IR_Id in Invocation_Relations.First ..
                   Invocation_Relations.Last
      loop
         Processor.all (IR_Id);
      end loop;
   end For_Each_Invocation_Relation;

   ----------------------------------
   -- For_Each_Invocation_Relation --
   ----------------------------------

   procedure For_Each_Invocation_Relation
     (U_Id      : Unit_Id;
      Processor : Invocation_Relation_Processor_Ptr)
   is
      pragma Assert (Present (U_Id));
      pragma Assert (Processor /= null);

      U_Rec : Unit_Record renames Units.Table (U_Id);

   begin
      for IR_Id in U_Rec.First_Invocation_Relation ..
                   U_Rec.Last_Invocation_Relation
      loop
         Processor.all (IR_Id);
      end loop;
   end For_Each_Invocation_Relation;

   ----------
   -- Hash --
   ----------

   function Hash
     (IS_Rec : Invocation_Signature_Record) return Bucket_Range_Type
   is
      function String_Hash is new System.String_Hash.Hash
        (Char_Type => Character,
         Key_Type  => String,
         Hash_Type => Bucket_Range_Type);

      Buffer : Bounded_String (2052);

   begin
      --  The hash is obtained from a signature based on the scope, name, line
      --  number, column number, and locations, in the following format:
      --
      --         scope__name__line_column__locations

      Append (Buffer, IS_Rec.Scope);
      Append (Buffer, "__");
      Append (Buffer, IS_Rec.Name);
      Append (Buffer, "__");
      Append (Buffer, IS_Rec.Line);
      Append (Buffer, '_');
      Append (Buffer, IS_Rec.Column);

      if IS_Rec.Locations /= No_Name then
         Append (Buffer, "__");
         Append (Buffer, IS_Rec.Locations);
      end if;

      return String_Hash (To_String (Buffer));
   end Hash;

   --------------------
   -- Initialize_ALI --
   --------------------

   procedure Initialize_ALI is
   begin
      --  When (re)initializing ALI data structures the ALI user expects to
      --  get a fresh set of data structures. Thus we first need to erase the
      --  marks put in the name table by the previous set of ALI routine calls.
      --  These two loops are empty and harmless the first time in.

      for J in ALIs.First .. ALIs.Last loop
         Set_Name_Table_Int (ALIs.Table (J).Afile, 0);
      end loop;

      for J in Units.First .. Units.Last loop
         Set_Name_Table_Int (Units.Table (J).Uname, 0);
      end loop;

      --  Free argument table strings

      for J in Args.First .. Args.Last loop
         Free (Args.Table (J));
      end loop;

      --  Initialize all tables

      ALIs.Init;
      Invocation_Constructs.Init;
      Invocation_Relations.Init;
      Invocation_Signatures.Init;
      Linker_Options.Init;
      No_Deps.Init;
      Notes.Init;
      Sdep.Init;
      Units.Init;
      Version_Ref.Reset;
      Withs.Init;
      Xref_Entity.Init;
      Xref.Init;
      Xref_Section.Init;

      --  Add dummy zeroth item in Linker_Options and Notes for sort calls

      Linker_Options.Increment_Last;
      Notes.Increment_Last;

      --  Initialize global variables recording cumulative options in all
      --  ALI files that are read for a given processing run in gnatbind.

      Dynamic_Elaboration_Checks_Specified   := False;
      Locking_Policy_Specified               := ' ';
      No_Normalize_Scalars_Specified         := False;
      No_Object_Specified                    := False;
      No_Component_Reordering_Specified      := False;
      GNATprove_Mode_Specified               := False;
      Normalize_Scalars_Specified            := False;
      Partition_Elaboration_Policy_Specified := ' ';
      Queuing_Policy_Specified               := ' ';
      SSO_Default_Specified                  := False;
      Task_Dispatching_Policy_Specified      := ' ';
      Unreserve_All_Interrupts_Specified     := False;
      Zero_Cost_Exceptions_Specified         := False;
   end Initialize_ALI;

   ---------------------------------------
   -- Invocation_Construct_Kind_To_Code --
   ---------------------------------------

   function Invocation_Construct_Kind_To_Code
     (Kind : Invocation_Construct_Kind) return Character
   is
   begin
      return Invocation_Construct_Codes (Kind);
   end Invocation_Construct_Kind_To_Code;

   -------------------------------
   -- Invocation_Graph_Encoding --
   -------------------------------

   function Invocation_Graph_Encoding return Invocation_Graph_Encoding_Kind is
   begin
      return Compile_Time_Invocation_Graph_Encoding;
   end Invocation_Graph_Encoding;

   --------------------------------------------
   -- Invocation_Graph_Encoding_Kind_To_Code --
   --------------------------------------------

   function Invocation_Graph_Encoding_Kind_To_Code
     (Kind : Invocation_Graph_Encoding_Kind) return Character
   is
   begin
      return Invocation_Graph_Encoding_Codes (Kind);
   end Invocation_Graph_Encoding_Kind_To_Code;

   ----------------------------------------
   -- Invocation_Graph_Line_Kind_To_Code --
   ----------------------------------------

   function Invocation_Graph_Line_Kind_To_Code
     (Kind : Invocation_Graph_Line_Kind) return Character
   is
   begin
      return Invocation_Graph_Line_Codes (Kind);
   end Invocation_Graph_Line_Kind_To_Code;

   -----------------------------
   -- Invocation_Kind_To_Code --
   -----------------------------

   function Invocation_Kind_To_Code
     (Kind : Invocation_Kind) return Character
   is
   begin
      return Invocation_Codes (Kind);
   end Invocation_Kind_To_Code;

   -----------------------------
   -- Invocation_Signature_Of --
   -----------------------------

   function Invocation_Signature_Of
     (Column    : Nat;
      Line      : Nat;
      Locations : Name_Id;
      Name      : Name_Id;
      Scope     : Name_Id) return Invocation_Signature_Id
   is
      IS_Rec : constant Invocation_Signature_Record :=
                 (Column    => Column,
                  Line      => Line,
                  Locations => Locations,
                  Name      => Name,
                  Scope     => Scope);
      IS_Id  : Invocation_Signature_Id;

   begin
      IS_Id := Sig_Map.Get (Sig_To_Sig_Map, IS_Rec);

      --  The invocation signature lacks an id. This indicates that it
      --  is encountered for the first time during the construction of
      --  the graph.

      if not Present (IS_Id) then
         Invocation_Signatures.Append (IS_Rec);
         IS_Id := Invocation_Signatures.Last;

         --  Map the invocation signature record to its corresponding id

         Sig_Map.Put (Sig_To_Sig_Map, IS_Rec, IS_Id);
      end if;

      return IS_Id;
   end Invocation_Signature_Of;

   -------------
   -- Invoker --
   -------------

   function Invoker
     (IR_Id : Invocation_Relation_Id) return Invocation_Signature_Id
   is
   begin
      pragma Assert (Present (IR_Id));
      return Invocation_Relations.Table (IR_Id).Invoker;
   end Invoker;

   ----------
   -- Kind --
   ----------

   function Kind
     (IC_Id : Invocation_Construct_Id) return Invocation_Construct_Kind
   is
   begin
      pragma Assert (Present (IC_Id));
      return Invocation_Constructs.Table (IC_Id).Kind;
   end Kind;

   ----------
   -- Kind --
   ----------

   function Kind (IR_Id : Invocation_Relation_Id) return Invocation_Kind is
   begin
      pragma Assert (Present (IR_Id));
      return Invocation_Relations.Table (IR_Id).Kind;
   end Kind;

   ----------
   -- Line --
   ----------

   function Line (IS_Id : Invocation_Signature_Id) return Nat is
   begin
      pragma Assert (Present (IS_Id));
      return Invocation_Signatures.Table (IS_Id).Line;
   end Line;

   ---------------
   -- Locations --
   ---------------

   function Locations (IS_Id : Invocation_Signature_Id) return Name_Id is
   begin
      pragma Assert (Present (IS_Id));
      return Invocation_Signatures.Table (IS_Id).Locations;
   end Locations;

   ----------
   -- Name --
   ----------

   function Name (IS_Id : Invocation_Signature_Id) return Name_Id is
   begin
      pragma Assert (Present (IS_Id));
      return Invocation_Signatures.Table (IS_Id).Name;
   end Name;

   -------------
   -- Present --
   -------------

   function Present (IC_Id : Invocation_Construct_Id) return Boolean is
   begin
      return IC_Id /= No_Invocation_Construct;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (IR_Id : Invocation_Relation_Id) return Boolean is
   begin
      return IR_Id /= No_Invocation_Relation;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (IS_Id : Invocation_Signature_Id) return Boolean is
   begin
      return IS_Id /= No_Invocation_Signature;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Dep : Sdep_Id) return Boolean is
   begin
      return Dep /= No_Sdep_Id;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (U_Id : Unit_Id) return Boolean is
   begin
      return U_Id /= No_Unit_Id;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (W_Id : With_Id) return Boolean is
   begin
      return W_Id /= No_With_Id;
   end Present;

   --------------
   -- Scan_ALI --
   --------------

   function Scan_ALI
     (F                : File_Name_Type;
      T                : Text_Buffer_Ptr;
      Err              : Boolean;
      Ignore_Lines     : String  := "X";
      Ignore_Errors    : Boolean := False;
      Directly_Scanned : Boolean := False) return ALI_Id
   is
      P         : Text_Ptr            := T'First;
      Line      : Logical_Line_Number := 1;
      Id        : ALI_Id;
      C         : Character;
      NS_Found  : Boolean;
      First_Arg : Arg_Id;

      Ignore : array (Character range 'A' .. 'Z') of Boolean :=
                 (others => False);
      --  Ignore (X) is set to True if lines starting with X are to
      --  be ignored by Scan_ALI and skipped, and False if the lines
      --  are to be read and processed.

      Bad_ALI_Format : exception;
      --  Exception raised by Fatal_Error if Err is True

      function At_Eol return Boolean;
      --  Test if at end of line

      function At_End_Of_Field return Boolean;
      --  Test if at end of line, or if at blank or horizontal tab

      procedure Check_At_End_Of_Field;
      --  Check if we are at end of field, fatal error if not

      procedure Checkc (C : Character);
      --  Check next character is C. If so bump past it, if not fatal error

      procedure Check_Unknown_Line;
      --  If Ignore_Errors mode, then checks C to make sure that it is not
      --  an unknown ALI line type characters, and if so, skips lines
      --  until the first character of the line is one of these characters,
      --  at which point it does a Getc to put that character in C. The
      --  call has no effect if C is already an appropriate character.
      --  If not in Ignore_Errors mode, a fatal error is signalled if the
      --  line is unknown. Note that if C is an EOL on entry, the line is
      --  skipped (it is assumed that blank lines are never significant).
      --  If C is EOF on entry, the call has no effect (it is assumed that
      --  the caller will properly handle this case).

      procedure Fatal_Error;
      --  Generate fatal error message for badly formatted ALI file if
      --  Err is false, or raise Bad_ALI_Format if Err is True.

      procedure Fatal_Error_Ignore;
      pragma Inline (Fatal_Error_Ignore);
      --  In Ignore_Errors mode, has no effect, otherwise same as Fatal_Error

      function Getc return Character;
      --  Get next character, bumping P past the character obtained

      function Get_File_Name
        (Lower         : Boolean := False;
         May_Be_Quoted : Boolean := False) return File_Name_Type;
      --  Skip blanks, then scan out a file name (name is left in Name_Buffer
      --  with length in Name_Len, as well as returning a File_Name_Type value.
      --  If May_Be_Quoted is True and the first non blank character is '"',
      --  then remove starting and ending quotes and undoubled internal quotes.
      --  If lower is false, the case is unchanged, if Lower is True then the
      --  result is forced to all lower case for systems where file names are
      --  not case sensitive. This ensures that gnatbind works correctly
      --  regardless of the case of the file name on all systems. The scan
      --  is terminated by a end of line, space or horizontal tab. Any other
      --  special characters are included in the returned name.

      function Get_Name
        (Ignore_Special : Boolean := False;
         May_Be_Quoted  : Boolean := False) return Name_Id;
      --  Skip blanks, then scan out a name (name is left in Name_Buffer with
      --  length in Name_Len, as well as being returned in Name_Id form).
      --  If Lower is set to True then the Name_Buffer will be converted to
      --  all lower case, for systems where file names are not case sensitive.
      --  This ensures that gnatbind works correctly regardless of the case
      --  of the file name on all systems.
      --
      --  The scan is terminated by the normal end of field condition
      --  (EOL, space, horizontal tab). Furthermore, the termination condition
      --  depends on the setting of Ignore_Special:
      --
      --    If Ignore_Special is False (normal case), the scan is terminated by
      --    a typeref bracket or an equal sign except for the special case of
      --    an operator name starting with a double quote that is terminated
      --    by another double quote.
      --
      --    If May_Be_Quoted is True and the first non blank character is '"'
      --    the name is 'unquoted'. In this case Ignore_Special is ignored and
      --    assumed to be True.
      --
      --  This function handles wide characters properly.

      function Get_Nat return Nat;
      --  Skip blanks, then scan out an unsigned integer value in Nat range
      --  raises ALI_Reading_Error if the encoutered type is not natural.

      function Get_Stamp return Time_Stamp_Type;
      --  Skip blanks, then scan out a time stamp

      function Get_Unit_Name return Unit_Name_Type;
      --  Skip blanks, then scan out a file name (name is left in Name_Buffer
      --  with length in Name_Len, as well as returning a Unit_Name_Type value.
      --  The case is unchanged and terminated by a normal end of field.

      function Nextc return Character;
      --  Return current character without modifying pointer P

      procedure Scan_Invocation_Graph_Line;
      --  Parse a single line that encodes a piece of the invocation graph

      procedure Skip_Eol;
      --  Skip past spaces, then skip past end of line (fatal error if not
      --  at end of line). Also skips past any following blank lines.

      procedure Skip_Line;
      --  Skip rest of current line and any following blank lines

      procedure Skip_Space;
      --  Skip past white space (blanks or horizontal tab)

      procedure Skipc;
      --  Skip past next character, does not affect value in C. This call
      --  is like calling Getc and ignoring the returned result.

      ---------------------
      -- At_End_Of_Field --
      ---------------------

      function At_End_Of_Field return Boolean is
      begin
         return Nextc <= ' ';
      end At_End_Of_Field;

      ------------
      -- At_Eol --
      ------------

      function At_Eol return Boolean is
      begin
         return Nextc = EOF or else Nextc = CR or else Nextc = LF;
      end At_Eol;

      ---------------------------
      -- Check_At_End_Of_Field --
      ---------------------------

      procedure Check_At_End_Of_Field is
      begin
         if not At_End_Of_Field then
            if Ignore_Errors then
               while Nextc > ' ' loop
                  P := P + 1;
               end loop;
            else
               Fatal_Error;
            end if;
         end if;
      end Check_At_End_Of_Field;

      ------------------------
      -- Check_Unknown_Line --
      ------------------------

      procedure Check_Unknown_Line is
      begin
         while C not in 'A' .. 'Z'
           or else not Known_ALI_Lines (C)
         loop
            if C = CR or else C = LF then
               Skip_Line;
               C := Nextc;

            elsif C = EOF then
               return;

            elsif Ignore_Errors then
               Skip_Line;
               C := Getc;

            else
               Fatal_Error;
            end if;
         end loop;
      end Check_Unknown_Line;

      ------------
      -- Checkc --
      ------------

      procedure Checkc (C : Character) is
      begin
         if Nextc = C then
            P := P + 1;
         elsif Ignore_Errors then
            P := P + 1;
         else
            Fatal_Error;
         end if;
      end Checkc;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error is
         Ptr1 : Text_Ptr;
         Ptr2 : Text_Ptr;
         Col  : Int;

         procedure Wchar (C : Character);
         --  Write a single character, replacing horizontal tab by spaces

         procedure Wchar (C : Character) is
         begin
            if C = HT then
               loop
                  Wchar (' ');
                  exit when Col mod 8 = 0;
               end loop;

            else
               Write_Char (C);
               Col := Col + 1;
            end if;
         end Wchar;

      --  Start of processing for Fatal_Error

      begin
         if Err then
            raise Bad_ALI_Format;
         end if;

         Set_Standard_Error;
         Write_Str ("fatal error: file ");
         Write_Name (F);
         Write_Str (" is incorrectly formatted");
         Write_Eol;

         Write_Str ("make sure you are using consistent versions " &

         --  Split the following line so that it can easily be transformed for
         --  other back-ends where the compiler might have a different name.

                    "of gcc/gnatbind");

         Write_Eol;

         --  Find start of line

         Ptr1 := P;
         while Ptr1 > T'First
           and then T (Ptr1 - 1) /= CR
           and then T (Ptr1 - 1) /= LF
         loop
            Ptr1 := Ptr1 - 1;
         end loop;

         Write_Int (Int (Line));
         Write_Str (". ");

         if Line < 100 then
            Write_Char (' ');
         end if;

         if Line < 10 then
            Write_Char (' ');
         end if;

         Col := 0;
         Ptr2 := Ptr1;

         while Ptr2 < T'Last
           and then T (Ptr2) /= CR
           and then T (Ptr2) /= LF
         loop
            Wchar (T (Ptr2));
            Ptr2 := Ptr2 + 1;
         end loop;

         Write_Eol;

         Write_Str ("     ");
         Col := 0;

         while Ptr1 < P loop
            if T (Ptr1) = HT then
               Wchar (HT);
            else
               Wchar (' ');
            end if;

            Ptr1 := Ptr1 + 1;
         end loop;

         Wchar ('|');
         Write_Eol;

         Exit_Program (E_Fatal);
      end Fatal_Error;

      ------------------------
      -- Fatal_Error_Ignore --
      ------------------------

      procedure Fatal_Error_Ignore is
      begin
         if not Ignore_Errors then
            Fatal_Error;
         end if;
      end Fatal_Error_Ignore;

      -------------------
      -- Get_File_Name --
      -------------------

      function Get_File_Name
        (Lower         : Boolean := False;
         May_Be_Quoted : Boolean := False) return File_Name_Type
      is
         F : Name_Id;

      begin
         F := Get_Name (Ignore_Special => True,
                        May_Be_Quoted  => May_Be_Quoted);

         --  Convert file name to all lower case if file names are not case
         --  sensitive. This ensures that we handle names in the canonical
         --  lower case format, regardless of the actual case.

         if Lower and not File_Names_Case_Sensitive then
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            return Name_Find;
         else
            return File_Name_Type (F);
         end if;
      end Get_File_Name;

      --------------
      -- Get_Name --
      --------------

      function Get_Name
        (Ignore_Special : Boolean := False;
         May_Be_Quoted  : Boolean := False) return Name_Id
      is
         Char : Character;

      begin
         Name_Len := 0;
         Skip_Space;

         if At_Eol then
            if Ignore_Errors then
               return Error_Name;
            else
               Fatal_Error;
            end if;
         end if;

         Char := Getc;

         --  Deal with quoted characters

         if May_Be_Quoted and then Char = '"' then
            loop
               if At_Eol then
                  if Ignore_Errors then
                     return Error_Name;
                  else
                     Fatal_Error;
                  end if;
               end if;

               Char := Getc;

               if Char = '"' then
                  if At_Eol then
                     exit;

                  else
                     Char := Getc;

                     if Char /= '"' then
                        P := P - 1;
                        exit;
                     end if;
                  end if;
               end if;

               Add_Char_To_Name_Buffer (Char);
            end loop;

         --  Other than case of quoted character

         else
            P := P - 1;
            loop
               Add_Char_To_Name_Buffer (Getc);

               exit when At_End_Of_Field;

               if not Ignore_Special then
                  if Name_Buffer (1) = '"' then
                     exit when Name_Len > 1
                               and then Name_Buffer (Name_Len) = '"';

                  else
                     --  Terminate on parens or angle brackets or equal sign

                     exit when Nextc = '(' or else Nextc = ')'
                       or else Nextc = '{' or else Nextc = '}'
                       or else Nextc = '<' or else Nextc = '>'
                       or else Nextc = '=';

                     --  Terminate on comma

                     exit when Nextc = ',';

                     --  Terminate if left bracket not part of wide char
                     --  sequence.

                     exit when Nextc = '[' and then T (P + 1) /= '"';

                     --  Terminate if right bracket not part of wide char
                     --  sequence.

                     exit when Nextc = ']' and then T (P - 1) /= '"';
                  end if;
               end if;
            end loop;
         end if;

         return Name_Find;
      end Get_Name;

      -------------------
      -- Get_Unit_Name --
      -------------------

      function Get_Unit_Name return Unit_Name_Type is
      begin
         return Unit_Name_Type (Get_Name);
      end Get_Unit_Name;

      -------------
      -- Get_Nat --
      -------------

      function Get_Nat return Nat is
         V : Nat;

      begin
         Skip_Space;

         --  Check if we are on a number. In the case of bad ALI files, this
         --  may not be true.

         if not (Nextc in '0' .. '9') then
            Fatal_Error;
         end if;

         V := 0;
         loop
            V := V * 10 + (Character'Pos (Getc) - Character'Pos ('0'));

            exit when At_End_Of_Field;
            exit when Nextc < '0' or else Nextc > '9';
         end loop;

         return V;
      end Get_Nat;

      ---------------
      -- Get_Stamp --
      ---------------

      function Get_Stamp return Time_Stamp_Type is
         T     : Time_Stamp_Type;
         Start : Integer;

      begin
         Skip_Space;

         if At_Eol then
            if Ignore_Errors then
               return Dummy_Time_Stamp;
            else
               Fatal_Error;
            end if;
         end if;

         --  Following reads old style time stamp missing first two digits

         if Nextc in '7' .. '9' then
            T (1) := '1';
            T (2) := '9';
            Start := 3;

         --  Normal case of full year in time stamp

         else
            Start := 1;
         end if;

         for J in Start .. T'Last loop
            T (J) := Getc;
         end loop;

         return T;
      end Get_Stamp;

      ----------
      -- Getc --
      ----------

      function Getc return Character is
      begin
         if P = T'Last then
            return EOF;
         else
            P := P + 1;
            return T (P - 1);
         end if;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         return T (P);
      end Nextc;

      --------------------------------
      -- Scan_Invocation_Graph_Line --
      --------------------------------

      procedure Scan_Invocation_Graph_Line is
         procedure Scan_Invocation_Construct_Line;
         pragma Inline (Scan_Invocation_Construct_Line);
         --  Parse an invocation construct line and construct the corresponding
         --  construct. The following data structures are updated:
         --
         --    * Invocation_Constructs
         --    * Units

         procedure Scan_Invocation_Graph_Attributes_Line;
         pragma Inline (Scan_Invocation_Graph_Attributes_Line);
         --  Parse an invocation-graph attributes line. The following data
         --  structures are updated:
         --
         --    * Units

         procedure Scan_Invocation_Relation_Line;
         pragma Inline (Scan_Invocation_Relation_Line);
         --  Parse an invocation relation line and construct the corresponding
         --  relation. The following data structures are updated:
         --
         --    * Invocation_Relations
         --    * Units

         function Scan_Invocation_Signature return Invocation_Signature_Id;
         pragma Inline (Scan_Invocation_Signature);
         --  Parse a single invocation signature while populating the following
         --  data structures:
         --
         --    * Invocation_Signatures
         --    * Sig_To_Sig_Map

         ------------------------------------
         -- Scan_Invocation_Construct_Line --
         ------------------------------------

         procedure Scan_Invocation_Construct_Line is
            Body_Placement : Declaration_Placement_Kind;
            Kind           : Invocation_Construct_Kind;
            Signature      : Invocation_Signature_Id;
            Spec_Placement : Declaration_Placement_Kind;

         begin
            --  construct-kind

            Kind := Code_To_Invocation_Construct_Kind (Getc);
            Checkc (' ');
            Skip_Space;

            --  construct-spec-placement

            Spec_Placement := Code_To_Declaration_Placement_Kind (Getc);
            Checkc (' ');
            Skip_Space;

            --  construct-body-placement

            Body_Placement := Code_To_Declaration_Placement_Kind (Getc);
            Checkc (' ');
            Skip_Space;

            --  construct-signature

            Signature := Scan_Invocation_Signature;
            Skip_Eol;

            Add_Invocation_Construct
              (Body_Placement => Body_Placement,
               Kind           => Kind,
               Signature      => Signature,
               Spec_Placement => Spec_Placement);
         end Scan_Invocation_Construct_Line;

         -------------------------------------------
         -- Scan_Invocation_Graph_Attributes_Line --
         -------------------------------------------

         procedure Scan_Invocation_Graph_Attributes_Line is
         begin
            --  encoding-kind

            Set_Invocation_Graph_Encoding
              (Code_To_Invocation_Graph_Encoding_Kind (Getc));
            Skip_Eol;
         end Scan_Invocation_Graph_Attributes_Line;

         -----------------------------------
         -- Scan_Invocation_Relation_Line --
         -----------------------------------

         procedure Scan_Invocation_Relation_Line is
            Extra   : Name_Id;
            Invoker : Invocation_Signature_Id;
            Kind    : Invocation_Kind;
            Target  : Invocation_Signature_Id;

         begin
            --  relation-kind

            Kind := Code_To_Invocation_Kind (Getc);
            Checkc (' ');
            Skip_Space;

            --  (extra-name | "none")

            Extra := Get_Name;

            if Extra = Name_None then
               Extra := No_Name;
            end if;

            Checkc (' ');
            Skip_Space;

            --  invoker-signature

            Invoker := Scan_Invocation_Signature;
            Checkc (' ');
            Skip_Space;

            --  target-signature

            Target := Scan_Invocation_Signature;
            Skip_Eol;

            Add_Invocation_Relation
              (Extra   => Extra,
               Invoker => Invoker,
               Kind    => Kind,
               Target  => Target);
         end Scan_Invocation_Relation_Line;

         -------------------------------
         -- Scan_Invocation_Signature --
         -------------------------------

         function Scan_Invocation_Signature return Invocation_Signature_Id is
            Column    : Nat;
            Line      : Nat;
            Locations : Name_Id;
            Name      : Name_Id;
            Scope     : Name_Id;

         begin
            --  [

            Checkc ('[');

            --  name

            Name := Get_Name;
            Checkc (' ');
            Skip_Space;

            --  scope

            Scope := Get_Name;
            Checkc (' ');
            Skip_Space;

            --  line

            Line := Get_Nat;
            Checkc (' ');
            Skip_Space;

            --  column

            Column := Get_Nat;
            Checkc (' ');
            Skip_Space;

            --  (locations | "none")

            Locations := Get_Name;

            if Locations = Name_None then
               Locations := No_Name;
            end if;

            --  ]

            Checkc (']');

            --  Create an invocation signature from the scanned attributes

            return
              Invocation_Signature_Of
                (Column    => Column,
                 Line      => Line,
                 Locations => Locations,
                 Name      => Name,
                 Scope     => Scope);
         end Scan_Invocation_Signature;

         --  Local variables

         Line : Invocation_Graph_Line_Kind;

      --  Start of processing for Scan_Invocation_Graph_Line

      begin
         if Ignore ('G') then
            return;
         end if;

         Checkc (' ');
         Skip_Space;

         --  line-kind

         Line := Code_To_Invocation_Graph_Line_Kind (Getc);
         Checkc (' ');
         Skip_Space;

         --  line-attributes

         case Line is
            when Invocation_Construct_Line =>
               Scan_Invocation_Construct_Line;

            when Invocation_Graph_Attributes_Line =>
               Scan_Invocation_Graph_Attributes_Line;

            when Invocation_Relation_Line =>
               Scan_Invocation_Relation_Line;
         end case;
      end Scan_Invocation_Graph_Line;

      --------------
      -- Skip_Eol --
      --------------

      procedure Skip_Eol is
      begin
         Skip_Space;

         if not At_Eol then
            if Ignore_Errors then
               while not At_Eol loop
                  P := P + 1;
               end loop;
            else
               Fatal_Error;
            end if;
         end if;

         --  Loop to skip past blank lines (first time through skips this EOL)

         while Nextc < ' ' and then Nextc /= EOF loop
            if Nextc = LF then
               Line := Line + 1;
            end if;

            P := P + 1;
         end loop;
      end Skip_Eol;

      ---------------
      -- Skip_Line --
      ---------------

      procedure Skip_Line is
      begin
         while not At_Eol loop
            P := P + 1;
         end loop;

         Skip_Eol;
      end Skip_Line;

      ----------------
      -- Skip_Space --
      ----------------

      procedure Skip_Space is
      begin
         while Nextc = ' ' or else Nextc = HT loop
            P := P + 1;
         end loop;
      end Skip_Space;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
      begin
         if P /= T'Last then
            P := P + 1;
         end if;
      end Skipc;

   --  Start of processing for Scan_ALI

   begin
      First_Sdep_Entry := Sdep.Last + 1;

      for J in Ignore_Lines'Range loop
         pragma Assert (Ignore_Lines (J) /= 'U');
         Ignore (Ignore_Lines (J)) := True;
      end loop;

      --  Setup ALI Table entry with appropriate defaults

      ALIs.Increment_Last;
      Id := ALIs.Last;
      Set_Name_Table_Int (F, Int (Id));

      ALIs.Table (Id) := (
        Afile                        => F,
        Compile_Errors               => False,
        First_CUDA_Kernel            => CUDA_Kernels.Last + 1,
        First_Interrupt_State        => Interrupt_States.Last + 1,
        First_Sdep                   => No_Sdep_Id,
        First_Specific_Dispatching   => Specific_Dispatching.Last + 1,
        First_Unit                   => No_Unit_Id,
        GNATprove_Mode               => False,
        Invocation_Graph_Encoding    => No_Encoding,
        Last_CUDA_Kernel             => CUDA_Kernels.Last,
        Last_Interrupt_State         => Interrupt_States.Last,
        Last_Sdep                    => No_Sdep_Id,
        Last_Specific_Dispatching    => Specific_Dispatching.Last,
        Last_Unit                    => No_Unit_Id,
        Locking_Policy               => ' ',
        Main_Priority                => -1,
        Main_CPU                     => -1,
        Main_Program                 => None,
        No_Component_Reordering      => False,
        No_Object                    => False,
        Normalize_Scalars            => False,
        Ofile_Full_Name              => Full_Object_File_Name,
        Partition_Elaboration_Policy => ' ',
        Queuing_Policy               => ' ',
        Restrictions                 => No_Restrictions,
        SAL_Interface                => False,
        Sfile                        => No_File,
        SSO_Default                  => ' ',
        Task_Dispatching_Policy      => ' ',
        Time_Slice_Value             => -1,
        WC_Encoding                  => 'b',
        Unit_Exception_Table         => False,
        Ver                          => (others => ' '),
        Ver_Len                      => 0,
        Zero_Cost_Exceptions         => False);

      --  Now we acquire the input lines from the ALI file. Note that the
      --  convention in the following code is that as we enter each section,
      --  C is set to contain the first character of the following line.

      C := Getc;
      Check_Unknown_Line;

      --  Acquire library version

      if C /= 'V' then

         --  The V line missing really indicates trouble, most likely it
         --  means we don't have an ALI file at all, so here we give a
         --  fatal error even if we are in Ignore_Errors mode.

         Fatal_Error;

      elsif Ignore ('V') then
         Skip_Line;

      else
         Checkc (' ');
         Skip_Space;
         Checkc ('"');

         for J in 1 .. Ver_Len_Max loop
            C := Getc;
            exit when C = '"';
            ALIs.Table (Id).Ver (J) := C;
            ALIs.Table (Id).Ver_Len := J;
         end loop;

         Skip_Eol;
      end if;

      C := Getc;
      Check_Unknown_Line;

      --  Acquire main program line if present

      if C = 'M' then
         if Ignore ('M') then
            Skip_Line;

         else
            Checkc (' ');
            Skip_Space;

            C := Getc;

            if C = 'F' then
               ALIs.Table (Id).Main_Program := Func;
            elsif C = 'P' then
               ALIs.Table (Id).Main_Program := Proc;
            else
               P := P - 1;
               Fatal_Error;
            end if;

            Skip_Space;

            if not At_Eol then
               if Nextc < 'A' then
                  ALIs.Table (Id).Main_Priority := Get_Nat;
               end if;

               Skip_Space;

               if Nextc = 'T' then
                  P := P + 1;
                  Checkc ('=');
                  ALIs.Table (Id).Time_Slice_Value := Get_Nat;
               end if;

               Skip_Space;

               if Nextc = 'C' then
                  P := P + 1;
                  Checkc ('=');
                  ALIs.Table (Id).Main_CPU := Get_Nat;
               end if;

               Skip_Space;

               Checkc ('W');
               Checkc ('=');
               ALIs.Table (Id).WC_Encoding := Getc;
            end if;

            Skip_Eol;
         end if;

         C := Getc;
      end if;

      --  Acquire argument lines

      First_Arg := Args.Last + 1;

      A_Loop : loop
         Check_Unknown_Line;
         exit A_Loop when C /= 'A';

         if Ignore ('A') then
            Skip_Line;

         else
            Checkc (' ');

            --  Scan out argument

            Name_Len := 0;
            while not At_Eol loop
               Add_Char_To_Name_Buffer (Getc);
            end loop;

            --  If -fstack-check, record that it occurred. Note that an
            --  additional string parameter can be specified, in the form of
            --  -fstack-check={no|generic|specific}. "no" means no checking,
            --  "generic" means force the use of old-style checking, and
            --  "specific" means use the best checking method.

            if Name_Len >= 13
              and then Name_Buffer (1 .. 13) = "-fstack-check"
              and then Name_Buffer (1 .. Name_Len) /= "-fstack-check=no"
            then
               Stack_Check_Switch_Set := True;
            end if;

            --  Store the argument

            Args.Increment_Last;
            Args.Table (Args.Last) := new String'(Name_Buffer (1 .. Name_Len));

            Skip_Eol;
         end if;

         C := Getc;
      end loop A_Loop;

      --  Acquire 'K' lines if present

      Check_Unknown_Line;

      while C = 'K' loop
         if Ignore ('K') then
            Skip_Line;

         else
            Skip_Space;
            CUDA_Kernels.Append ((Kernel_Name => Get_Name));
            ALIs.Table (Id).Last_CUDA_Kernel := CUDA_Kernels.Last;
            Skip_Eol;
         end if;

         C := Getc;
      end loop;

      --  Acquire P line

      Check_Unknown_Line;

      while C /= 'P' loop
         if Ignore_Errors then
            if C = EOF then
               Fatal_Error;
            else
               Skip_Line;
               C := Nextc;
            end if;
         else
            Fatal_Error;
         end if;
      end loop;

      if Ignore ('P') then
         Skip_Line;

      --  Process P line

      else
         NS_Found := False;

         while not At_Eol loop
            Checkc (' ');
            Skip_Space;
            C := Getc;

            --  Processing for CE

            if C = 'C' then
               Checkc ('E');
               ALIs.Table (Id).Compile_Errors := True;

            --  Processing for DB

            elsif C = 'D' then
               Checkc ('B');
               Detect_Blocking := True;

            --  Processing for Ex

            elsif C = 'E' then
               Partition_Elaboration_Policy_Specified := Getc;
               ALIs.Table (Id).Partition_Elaboration_Policy :=
                 Partition_Elaboration_Policy_Specified;

            --  Processing for FX

            elsif C = 'F' then
               C := Getc;

               --  Old front-end exceptions marker, ignore

               if C = 'X' then
                  null;
               else
                  Fatal_Error_Ignore;
               end if;

            --  Processing for GP

            elsif C = 'G' then
               Checkc ('P');
               GNATprove_Mode_Specified := True;
               ALIs.Table (Id).GNATprove_Mode := True;

            --  Processing for Lx

            elsif C = 'L' then
               Locking_Policy_Specified := Getc;
               ALIs.Table (Id).Locking_Policy := Locking_Policy_Specified;

            --  Processing for flags starting with N

            elsif C = 'N' then
               C := Getc;

               --  Processing for NC

               if C = 'C' then
                  ALIs.Table (Id).No_Component_Reordering := True;
                  No_Component_Reordering_Specified := True;

               --  Processing for NO

               elsif C = 'O' then
                  ALIs.Table (Id).No_Object := True;
                  No_Object_Specified := True;

               --  Processing for NR

               elsif C = 'R' then
                  No_Run_Time_Mode           := True;
                  Configurable_Run_Time_Mode := True;

               --  Processing for NS

               elsif C = 'S' then
                  ALIs.Table (Id).Normalize_Scalars := True;
                  Normalize_Scalars_Specified := True;
                  NS_Found := True;

               --  Invalid switch starting with N

               else
                  Fatal_Error_Ignore;
               end if;

            --  Processing for OH/OL

            elsif C = 'O' then
               C := Getc;

               if C = 'L' or else C = 'H' then
                  ALIs.Table (Id).SSO_Default := C;
                  SSO_Default_Specified := True;

               else
                  Fatal_Error_Ignore;
               end if;

            --  Processing for Qx

            elsif C = 'Q' then
               Queuing_Policy_Specified := Getc;
               ALIs.Table (Id).Queuing_Policy := Queuing_Policy_Specified;

            --  Processing for flags starting with S

            elsif C = 'S' then
               C := Getc;

               --  Processing for SL

               if C = 'L' then
                  ALIs.Table (Id).SAL_Interface := True;

               --  Processing for SS

               elsif C = 'S' then
                  --  Special case: a-tags/i-c* by themselves should not set
                  --  Sec_Stack_Used, only if other code uses the secondary
                  --  stack should we set this flag. This ensures that we do
                  --  not bring the secondary stack unnecessarily when using
                  --  one of these packages and not actually using the
                  --  secondary stack.

                  declare
                     File : constant String := Get_Name_String (F);
                  begin
                     if File /= "a-tags.ali"
                       and then File /= "i-c.ali"
                       and then File /= "i-cstrin.ali"
                       and then File /= "i-cpoint.ali"
                     then
                        Opt.Sec_Stack_Used := True;
                     end if;
                  end;

               --  Invalid switch starting with S

               else
                  Fatal_Error_Ignore;
               end if;

            --  Processing for Tx

            elsif C = 'T' then
               Task_Dispatching_Policy_Specified := Getc;
               ALIs.Table (Id).Task_Dispatching_Policy :=
                 Task_Dispatching_Policy_Specified;

            --  Processing for switch starting with U

            elsif C = 'U' then
               C := Getc;

               --  Processing for UA

               if C  = 'A' then
                  Unreserve_All_Interrupts_Specified := True;

               --  Processing for UX

               elsif C = 'X' then
                  ALIs.Table (Id).Unit_Exception_Table := True;

               --  Invalid switches starting with U

               else
                  Fatal_Error_Ignore;
               end if;

            --  Processing for ZX

            elsif C = 'Z' then
               C := Getc;

               if C = 'X' then
                  ALIs.Table (Id).Zero_Cost_Exceptions := True;
                  Zero_Cost_Exceptions_Specified := True;
               else
                  Fatal_Error_Ignore;
               end if;

            --  Invalid parameter

            else
               C := Getc;
               Fatal_Error_Ignore;
            end if;
         end loop;

         if not NS_Found then
            No_Normalize_Scalars_Specified := True;
         end if;

         Skip_Eol;
      end if;

      C := Getc;
      Check_Unknown_Line;

      --  Loop to skip to first restrictions line

      while C /= 'R' loop
         if Ignore_Errors then
            if C = EOF then
               Fatal_Error;
            else
               Skip_Line;
               C := Nextc;
            end if;
         else
            Fatal_Error;
         end if;
      end loop;

      --  Ignore all 'R' lines if that is required

      if Ignore ('R') then
         while C = 'R' loop
            Skip_Line;
            C := Getc;
         end loop;

      --  Here we process the restrictions lines (other than unit name cases)

      else
         Scan_Restrictions : declare
            Save_R : constant Restrictions_Info := Cumulative_Restrictions;
            --  Save cumulative restrictions in case we have a fatal error

            Bad_R_Line : exception;
            --  Signal bad restrictions line (raised on unexpected character)

            Typ : Character;
            R   : Restriction_Id;
            N   : Natural;

         begin
            --  Named restriction case

            if Nextc = 'N' then
               Skip_Line;
               C := Getc;

               --  Loop through RR and RV lines

               while C = 'R' and then Nextc /= ' ' loop
                  Typ := Getc;
                  Checkc (' ');

                  --  Acquire restriction name

                  Name_Len := 0;
                  while not At_Eol and then Nextc /= '=' loop
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := Getc;
                  end loop;

                  --  Now search list of restrictions to find match

                  declare
                     RN : String renames Name_Buffer (1 .. Name_Len);

                  begin
                     R := Restriction_Id'First;
                     while R /= Not_A_Restriction_Id loop
                        if Restriction_Id'Image (R) = RN then
                           goto R_Found;
                        end if;

                        R := Restriction_Id'Succ (R);
                     end loop;

                     --  We don't recognize the restriction. This might be
                     --  thought of as an error, and it really is, but we
                     --  want to allow building with inconsistent versions
                     --  of the binder and ali files (see comments at the
                     --  start of package System.Rident), so we just ignore
                     --  this situation.

                     goto Done_With_Restriction_Line;
                  end;

                  <<R_Found>>

                  case R is

                     --  Boolean restriction case

                     when All_Boolean_Restrictions =>
                        case Typ is
                           when 'V' =>
                              ALIs.Table (Id).Restrictions.Violated (R) :=
                                True;
                              Cumulative_Restrictions.Violated (R) := True;

                           when 'R' =>
                              ALIs.Table (Id).Restrictions.Set (R) := True;
                              Cumulative_Restrictions.Set (R) := True;

                           when others =>
                              raise Bad_R_Line;
                        end case;

                     --  Parameter restriction case

                     when All_Parameter_Restrictions =>
                        if At_Eol or else Nextc /= '=' then
                           raise Bad_R_Line;
                        else
                           Skipc;
                        end if;

                        N := Natural (Get_Nat);

                        case Typ is

                           --  Restriction set

                           when 'R' =>
                              ALIs.Table (Id).Restrictions.Set (R) := True;
                              ALIs.Table (Id).Restrictions.Value (R) := N;

                              if Cumulative_Restrictions.Set (R) then
                                 Cumulative_Restrictions.Value (R) :=
                                   Integer'Min
                                     (Cumulative_Restrictions.Value (R), N);
                              else
                                 Cumulative_Restrictions.Set (R) := True;
                                 Cumulative_Restrictions.Value (R) := N;
                              end if;

                           --  Restriction violated

                           when 'V' =>
                              ALIs.Table (Id).Restrictions.Violated (R) :=
                                True;
                              Cumulative_Restrictions.Violated (R) := True;
                              ALIs.Table (Id).Restrictions.Count (R) := N;

                              --  Checked Max_Parameter case

                              if R in Checked_Max_Parameter_Restrictions then
                                 Cumulative_Restrictions.Count (R) :=
                                   Integer'Max
                                     (Cumulative_Restrictions.Count (R), N);

                              --  Other checked parameter cases

                              else
                                 declare
                                    pragma Unsuppress (Overflow_Check);

                                 begin
                                    Cumulative_Restrictions.Count (R) :=
                                      Cumulative_Restrictions.Count (R) + N;

                                 exception
                                    when Constraint_Error =>

                                       --  A constraint error comes from the
                                       --  addition. We reset to the maximum
                                       --  and indicate that the real value
                                       --  is now unknown.

                                       Cumulative_Restrictions.Value (R) :=
                                         Integer'Last;
                                       Cumulative_Restrictions.Unknown (R) :=
                                         True;
                                 end;
                              end if;

                              --  Deal with + case

                              if Nextc = '+' then
                                 Skipc;
                                 ALIs.Table (Id).Restrictions.Unknown (R) :=
                                   True;
                                 Cumulative_Restrictions.Unknown (R) := True;
                              end if;

                           --  Other than 'R' or 'V'

                           when others =>
                              raise Bad_R_Line;
                        end case;

                        if not At_Eol then
                           raise Bad_R_Line;
                        end if;

                     --  Bizarre error case NOT_A_RESTRICTION

                     when Not_A_Restriction_Id =>
                        raise Bad_R_Line;
                  end case;

                  if not At_Eol then
                     raise Bad_R_Line;
                  end if;

               <<Done_With_Restriction_Line>>
                  Skip_Line;
                  C := Getc;
               end loop;

            --  Positional restriction case

            else
               Checkc (' ');
               Skip_Space;

               --  Acquire information for boolean restrictions

               for R in All_Boolean_Restrictions loop
                  C := Getc;

                  case C is
                     when 'v' =>
                        ALIs.Table (Id).Restrictions.Violated (R) := True;
                        Cumulative_Restrictions.Violated (R) := True;

                     when 'r' =>
                        ALIs.Table (Id).Restrictions.Set (R) := True;
                        Cumulative_Restrictions.Set (R) := True;

                     when 'n' =>
                        null;

                     when others =>
                        raise Bad_R_Line;
                  end case;
               end loop;

               --  Acquire information for parameter restrictions

               for RP in All_Parameter_Restrictions loop
                  case Getc is
                     when 'n' =>
                        null;

                     when 'r' =>
                        ALIs.Table (Id).Restrictions.Set (RP) := True;

                        declare
                           N : constant Integer := Integer (Get_Nat);
                        begin
                           ALIs.Table (Id).Restrictions.Value (RP) := N;

                           if Cumulative_Restrictions.Set (RP) then
                              Cumulative_Restrictions.Value (RP) :=
                                Integer'Min
                                  (Cumulative_Restrictions.Value (RP), N);
                           else
                              Cumulative_Restrictions.Set (RP) := True;
                              Cumulative_Restrictions.Value (RP) := N;
                           end if;
                        end;

                     when others =>
                        raise Bad_R_Line;
                  end case;

                  --  Acquire restrictions violations information

                  case Getc is

                  when 'n' =>
                     null;

                  when 'v' =>
                     ALIs.Table (Id).Restrictions.Violated (RP) := True;
                     Cumulative_Restrictions.Violated (RP) := True;

                     declare
                        N : constant Integer := Integer (Get_Nat);

                     begin
                        ALIs.Table (Id).Restrictions.Count (RP) := N;

                        if RP in Checked_Max_Parameter_Restrictions then
                           Cumulative_Restrictions.Count (RP) :=
                             Integer'Max
                               (Cumulative_Restrictions.Count (RP), N);

                        else
                           declare
                              pragma Unsuppress (Overflow_Check);

                           begin
                              Cumulative_Restrictions.Count (RP) :=
                                Cumulative_Restrictions.Count (RP) + N;

                           exception
                              when Constraint_Error =>

                                 --  A constraint error comes from the add. We
                                 --  reset to the maximum and indicate that the
                                 --  real value is now unknown.

                                 Cumulative_Restrictions.Value (RP) :=
                                   Integer'Last;
                                 Cumulative_Restrictions.Unknown (RP) := True;
                           end;
                        end if;

                        if Nextc = '+' then
                           Skipc;
                           ALIs.Table (Id).Restrictions.Unknown (RP) := True;
                           Cumulative_Restrictions.Unknown (RP) := True;
                        end if;
                     end;

                  when others =>
                     raise Bad_R_Line;
                  end case;
               end loop;

               if not At_Eol then
                  raise Bad_R_Line;
               else
                  Skip_Line;
                  C := Getc;
               end if;
            end if;

         --  Here if error during scanning of restrictions line

         exception
            when Bad_R_Line =>

               --  In Ignore_Errors mode, undo any changes to restrictions
               --  from this unit, and continue on, skipping remaining R
               --  lines for this unit.

               if Ignore_Errors then
                  Cumulative_Restrictions := Save_R;
                  ALIs.Table (Id).Restrictions := No_Restrictions;

                  loop
                     Skip_Eol;
                     C := Getc;
                     exit when C /= 'R';
                  end loop;

               --  In normal mode, this is a fatal error

               else
                  Fatal_Error;
               end if;
         end Scan_Restrictions;
      end if;

      --  Acquire additional restrictions (No_Dependence) lines if present

      while C = 'R' loop
         if Ignore ('R') then
            Skip_Line;
         else
            Skip_Space;
            No_Deps.Append ((Id, Get_Name));
            Skip_Eol;
         end if;

         C := Getc;
      end loop;

      --  Acquire 'I' lines if present

      Check_Unknown_Line;

      while C = 'I' loop
         if Ignore ('I') then
            Skip_Line;

         else
            declare
               Int_Num : Nat;
               I_State : Character;
               Line_No : Nat;

            begin
               Int_Num := Get_Nat;
               Skip_Space;
               I_State := Getc;
               Line_No := Get_Nat;

               Interrupt_States.Append (
                 (Interrupt_Id    => Int_Num,
                  Interrupt_State => I_State,
                  IS_Pragma_Line  => Line_No));

               ALIs.Table (Id).Last_Interrupt_State := Interrupt_States.Last;
               Skip_Eol;
            end;
         end if;

         C := Getc;
      end loop;

      --  Acquire 'S' lines if present

      Check_Unknown_Line;

      while C = 'S' loop
         if Ignore ('S') then
            Skip_Line;

         else
            declare
               Policy     : Character;
               First_Prio : Nat;
               Last_Prio  : Nat;
               Line_No    : Nat;

            begin
               Checkc (' ');
               Skip_Space;

               Policy := Getc;
               Skip_Space;
               First_Prio := Get_Nat;
               Last_Prio := Get_Nat;
               Line_No := Get_Nat;

               Specific_Dispatching.Append (
                 (Dispatching_Policy => Policy,
                  First_Priority     => First_Prio,
                  Last_Priority      => Last_Prio,
                  PSD_Pragma_Line    => Line_No));

               ALIs.Table (Id).Last_Specific_Dispatching :=
                 Specific_Dispatching.Last;

               Skip_Eol;
            end;
         end if;

         C := Getc;
      end loop;

      --  Loop to acquire unit entries

      U_Loop : loop
         Check_Unknown_Line;
         exit U_Loop when C /= 'U';

         --  Note: as per spec, we never ignore U lines

         Checkc (' ');
         Skip_Space;
         Units.Increment_Last;

         if ALIs.Table (Id).First_Unit = No_Unit_Id then
            ALIs.Table (Id).First_Unit := Units.Last;
         end if;

         declare
            UL : Unit_Record renames Units.Table (Units.Last);

         begin
            UL.Uname                      := Get_Unit_Name;
            UL.Predefined                 := Is_Predefined_Unit;
            UL.Internal                   := Is_Internal_Unit;
            UL.My_ALI                     := Id;
            UL.Sfile                      := Get_File_Name (Lower => True);
            UL.Pure                       := False;
            UL.Preelab                    := False;
            UL.No_Elab                    := False;
            UL.Shared_Passive             := False;
            UL.RCI                        := False;
            UL.Remote_Types               := False;
            UL.Serious_Errors             := False;
            UL.Has_RACW                   := False;
            UL.Init_Scalars               := False;
            UL.Is_Generic                 := False;
            UL.Icasing                    := Mixed_Case;
            UL.Kcasing                    := All_Lower_Case;
            UL.Dynamic_Elab               := False;
            UL.Elaborate_Body             := False;
            UL.Set_Elab_Entity            := False;
            UL.Version                    := "00000000";
            UL.First_With                 := Withs.Last + 1;
            UL.First_Arg                  := First_Arg;
            UL.First_Invocation_Construct := Invocation_Constructs.Last + 1;
            UL.Last_Invocation_Construct  := No_Invocation_Construct;
            UL.First_Invocation_Relation  := Invocation_Relations.Last + 1;
            UL.Last_Invocation_Relation   := No_Invocation_Relation;
            UL.Elab_Position              := 0;
            UL.SAL_Interface              := ALIs.Table (Id).SAL_Interface;
            UL.Directly_Scanned           := Directly_Scanned;
            UL.Body_Needed_For_SAL        := False;
            UL.Elaborate_Body_Desirable   := False;
            UL.Optimize_Alignment         := 'O';
            UL.Has_Finalizer              := False;
            UL.Primary_Stack_Count        := 0;
            UL.Sec_Stack_Count            := 0;

            if Debug_Flag_U then
               Write_Str (" ----> reading unit ");
               Write_Int (Int (Units.Last));
               Write_Str ("  ");
               Write_Unit_Name (UL.Uname);
               Write_Str (" from file ");
               Write_Name (UL.Sfile);
               Write_Eol;
            end if;
         end;

         --  Check for duplicated unit in different files

         declare
            Info : constant Int := Get_Name_Table_Int
                                     (Units.Table (Units.Last).Uname);
         begin
            if Info /= 0
              and then Units.Table (Units.Last).Sfile /=
                       Units.Table (Unit_Id (Info)).Sfile
            then
               --  If Err is set then ignore duplicate unit name. This is the
               --  case of a call from gnatmake, where the situation can arise
               --  from substitution of source files. In such situations, the
               --  processing in gnatmake will always result in any required
               --  recompilations in any case, and if we consider this to be
               --  an error we get strange cases (for example when a generic
               --  instantiation is replaced by a normal package) where we
               --  read the old ali file, decide to recompile, and then decide
               --  that the old and new ali files are incompatible.

               if Err then
                  null;

               --  If Err is not set, then this is a fatal error. This is
               --  the case of being called from the binder, where we must
               --  definitely diagnose this as an error.

               else
                  Set_Standard_Error;
                  Write_Str ("error: duplicate unit name: ");
                  Write_Eol;

                  Write_Str ("error: unit """);
                  Write_Unit_Name (Units.Table (Units.Last).Uname);
                  Write_Str (""" found in file """);
                  Write_Name_Decoded (Units.Table (Units.Last).Sfile);
                  Write_Char ('"');
                  Write_Eol;

                  Write_Str ("error: unit """);
                  Write_Unit_Name (Units.Table (Unit_Id (Info)).Uname);
                  Write_Str (""" found in file """);
                  Write_Name_Decoded (Units.Table (Unit_Id (Info)).Sfile);
                  Write_Char ('"');
                  Write_Eol;

                  Exit_Program (E_Fatal);
               end if;
            end if;
         end;

         Set_Name_Table_Int
           (Units.Table (Units.Last).Uname, Int (Units.Last));

         --  Scan out possible version and other parameters

         loop
            Skip_Space;
            exit when At_Eol;
            C := Getc;

            --  Version field

            if C in '0' .. '9' or else C in 'a' .. 'f' then
               Units.Table (Units.Last).Version (1) := C;

               for J in 2 .. 8 loop
                  C := Getc;
                  Units.Table (Units.Last).Version (J) := C;
               end loop;

            --  BD/BN parameters

            elsif C = 'B' then
               C := Getc;

               if C = 'D' then
                  Check_At_End_Of_Field;
                  Units.Table (Units.Last).Elaborate_Body_Desirable := True;

               elsif C = 'N' then
                  Check_At_End_Of_Field;
                  Units.Table (Units.Last).Body_Needed_For_SAL := True;

               else
                  Fatal_Error_Ignore;
               end if;

            --  DE parameter (Dynamic elaboration checks)

            elsif C = 'D' then
               C := Getc;

               if C = 'E' then
                  Check_At_End_Of_Field;
                  Units.Table (Units.Last).Dynamic_Elab := True;
                  Dynamic_Elaboration_Checks_Specified := True;
               else
                  Fatal_Error_Ignore;
               end if;

            --  EB/EE parameters

            elsif C = 'E' then
               C := Getc;

               if C = 'B' then
                  Units.Table (Units.Last).Elaborate_Body := True;
               elsif C = 'E' then
                  Units.Table (Units.Last).Set_Elab_Entity := True;
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  GE parameter (generic)

            elsif C = 'G' then
               C := Getc;

               if C = 'E' then
                  Check_At_End_Of_Field;
                  Units.Table (Units.Last).Is_Generic := True;
               else
                  Fatal_Error_Ignore;
               end if;

            --  IL/IS/IU parameters

            elsif C = 'I' then
               C := Getc;

               if C = 'L' then
                  Units.Table (Units.Last).Icasing := All_Lower_Case;
               elsif C = 'S' then
                  Units.Table (Units.Last).Init_Scalars := True;
                  Initialize_Scalars_Used := True;
               elsif C = 'U' then
                  Units.Table (Units.Last).Icasing := All_Upper_Case;
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  KM/KU parameters

            elsif C = 'K' then
               C := Getc;

               if C = 'M' then
                  Units.Table (Units.Last).Kcasing := Mixed_Case;
               elsif C = 'U' then
                  Units.Table (Units.Last).Kcasing := All_Upper_Case;
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  NE parameter

            elsif C = 'N' then
               C := Getc;

               if C = 'E' then
                  Units.Table (Units.Last).No_Elab := True;
                  Check_At_End_Of_Field;
               else
                  Fatal_Error_Ignore;
               end if;

            --  PF/PR/PU/PK parameters

            elsif C = 'P' then
               C := Getc;

               if C = 'F' then
                  Units.Table (Units.Last).Has_Finalizer := True;
               elsif C = 'R' then
                  Units.Table (Units.Last).Preelab := True;
               elsif C = 'U' then
                  Units.Table (Units.Last).Pure := True;
               elsif C = 'K' then
                  Units.Table (Units.Last).Unit_Kind := 'p';
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  OL/OO/OS/OT parameters

            elsif C = 'O' then
               C := Getc;

               if C = 'L' or else C = 'O' or else C = 'S' or else C = 'T' then
                  Units.Table (Units.Last).Optimize_Alignment := C;
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  RC/RT parameters

            elsif C = 'R' then
               C := Getc;

               if C = 'C' then
                  Units.Table (Units.Last).RCI := True;
               elsif C = 'T' then
                  Units.Table (Units.Last).Remote_Types := True;
               elsif C = 'A' then
                  Units.Table (Units.Last).Has_RACW := True;
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            --  SE/SP/SU parameters

            elsif C = 'S' then
               C := Getc;

               if C = 'E' then
                  Units.Table (Units.Last).Serious_Errors := True;
               elsif C = 'P' then
                  Units.Table (Units.Last).Shared_Passive := True;
               elsif C = 'U' then
                  Units.Table (Units.Last).Unit_Kind := 's';
               else
                  Fatal_Error_Ignore;
               end if;

               Check_At_End_Of_Field;

            else
               C := Getc;
               Fatal_Error_Ignore;
            end if;
         end loop;

         Skip_Eol;

         C := Getc;

         --  Scan out With lines for this unit

         With_Loop : loop
            Check_Unknown_Line;
            exit With_Loop when C /= 'W' and then C /= 'Y' and then C /= 'Z';

            if Ignore ('W') then
               Skip_Line;

            else
               Checkc (' ');
               Skip_Space;
               Withs.Increment_Last;
               Withs.Table (Withs.Last).Uname              := Get_Unit_Name;
               Withs.Table (Withs.Last).Elaborate          := False;
               Withs.Table (Withs.Last).Elaborate_All      := False;
               Withs.Table (Withs.Last).Elab_Desirable     := False;
               Withs.Table (Withs.Last).Elab_All_Desirable := False;
               Withs.Table (Withs.Last).SAL_Interface      := False;
               Withs.Table (Withs.Last).Limited_With       := (C = 'Y');
               Withs.Table (Withs.Last).Implicit_With      := (C = 'Z');

               --  Generic case with no object file available

               if At_Eol then
                  Withs.Table (Withs.Last).Sfile := No_File;
                  Withs.Table (Withs.Last).Afile := No_File;

               --  Normal case

               else
                  Withs.Table (Withs.Last).Sfile := Get_File_Name
                                                      (Lower => True);
                  Withs.Table (Withs.Last).Afile := Get_File_Name
                                                      (Lower => True);

                  --  Scan out possible E, EA, ED, and AD parameters

                  while not At_Eol loop
                     Skip_Space;

                     if Nextc = 'A' then
                        P := P + 1;
                        Checkc ('D');
                        Check_At_End_Of_Field;

                        --  Store AD indication unless ignore required

                        Withs.Table (Withs.Last).Elab_All_Desirable := True;

                     elsif Nextc = 'E' then
                        P := P + 1;

                        if At_End_Of_Field then
                           Withs.Table (Withs.Last).Elaborate := True;

                        elsif Nextc = 'A' then
                           P := P + 1;
                           Check_At_End_Of_Field;
                           Withs.Table (Withs.Last).Elaborate_All := True;

                        else
                           Checkc ('D');
                           Check_At_End_Of_Field;

                           --  Store ED indication

                           Withs.Table (Withs.Last).Elab_Desirable := True;
                        end if;

                     else
                        Fatal_Error;
                     end if;
                  end loop;
               end if;

               Skip_Eol;
            end if;

            C := Getc;
         end loop With_Loop;

         Units.Table (Units.Last).Last_With := Withs.Last;
         Units.Table (Units.Last).Last_Arg  := Args.Last;

         --  Scan out task stack information for the unit if present

         Check_Unknown_Line;

         if C = 'T' then
            if Ignore ('T') then
               Skip_Line;

            else
               Checkc (' ');
               Skip_Space;

               Units.Table (Units.Last).Primary_Stack_Count := Get_Nat;
               Skip_Space;
               Units.Table (Units.Last).Sec_Stack_Count := Get_Nat;
               Skip_Space;
               Skip_Eol;
            end if;

            C := Getc;
         end if;

         --  If there are linker options lines present, scan them

         Name_Len := 0;

         Linker_Options_Loop : loop
            Check_Unknown_Line;
            exit Linker_Options_Loop when C /= 'L';

            if Ignore ('L') then
               Skip_Line;

            else
               Checkc (' ');
               Skip_Space;
               Checkc ('"');

               loop
                  C := Getc;

                  if C < Character'Val (16#20#)
                    or else C > Character'Val (16#7E#)
                  then
                     Fatal_Error_Ignore;

                  elsif C = '{' then
                     C := Character'Val (0);

                     declare
                        V : Natural;

                     begin
                        V := 0;
                        for J in 1 .. 2 loop
                           C := Getc;

                           if C in '0' .. '9' then
                              V := V * 16 +
                                     Character'Pos (C) -
                                       Character'Pos ('0');

                           elsif C in 'A' .. 'F' then
                              V := V * 16 +
                                     Character'Pos (C) -
                                       Character'Pos ('A') +
                                         10;

                           else
                              Fatal_Error_Ignore;
                           end if;
                        end loop;

                        Checkc ('}');
                        Add_Char_To_Name_Buffer (Character'Val (V));
                     end;

                  else
                     if C = '"' then
                        exit when Nextc /= '"';
                        C := Getc;
                     end if;

                     Add_Char_To_Name_Buffer (C);
                  end if;
               end loop;

               Add_Char_To_Name_Buffer (NUL);
               Skip_Eol;
            end if;

            C := Getc;
         end loop Linker_Options_Loop;

         --  Store the linker options entry if one was found

         if Name_Len /= 0 then
            Linker_Options.Increment_Last;

            Linker_Options.Table (Linker_Options.Last).Name :=
              Name_Enter;

            Linker_Options.Table (Linker_Options.Last).Unit :=
              Units.Last;

            Linker_Options.Table (Linker_Options.Last).Internal_File :=
              Is_Internal_File_Name (F);
         end if;

         --  If there are notes present, scan them

         Notes_Loop : loop
            Check_Unknown_Line;
            exit Notes_Loop when C /= 'N';

            if Ignore ('N') then
               Skip_Line;

            else
               Checkc (' ');

               Notes.Increment_Last;
               Notes.Table (Notes.Last).Pragma_Type := Getc;
               Notes.Table (Notes.Last).Pragma_Line := Get_Nat;
               Checkc (':');
               Notes.Table (Notes.Last).Pragma_Col  := Get_Nat;

               if not At_Eol and then Nextc = ':' then
                  Checkc (':');
                  Notes.Table (Notes.Last).Pragma_Source_File :=
                    Get_File_Name (Lower => True);
               else
                  Notes.Table (Notes.Last).Pragma_Source_File :=
                    Units.Table (Units.Last).Sfile;
               end if;

               if At_Eol then
                  Notes.Table (Notes.Last).Pragma_Args := No_Name;

               else
                  --  Note: can't use Get_Name here as the remainder of the
                  --  line is unstructured text whose syntax depends on the
                  --  particular pragma used.

                  Checkc (' ');

                  Name_Len := 0;
                  while not At_Eol loop
                     Add_Char_To_Name_Buffer (Getc);
                  end loop;
               end if;

               Skip_Eol;
            end if;

            C := Getc;
         end loop Notes_Loop;
      end loop U_Loop;

      --  End loop through units for one ALI file

      ALIs.Table (Id).Last_Unit := Units.Last;
      ALIs.Table (Id).Sfile := Units.Table (ALIs.Table (Id).First_Unit).Sfile;

      --  Set types of the units (there can be at most 2 of them)

      if ALIs.Table (Id).First_Unit /= ALIs.Table (Id).Last_Unit then
         Units.Table (ALIs.Table (Id).First_Unit).Utype := Is_Body;
         Units.Table (ALIs.Table (Id).Last_Unit).Utype  := Is_Spec;

      else
         --  Deal with body only and spec only cases, note that the reason we
         --  do our own checking of the name (rather than using Is_Body_Name)
         --  is that Uname drags in far too much compiler junk.

         Get_Name_String (Units.Table (Units.Last).Uname);

         if Name_Buffer (Name_Len) = 'b' then
            Units.Table (Units.Last).Utype := Is_Body_Only;
         else
            Units.Table (Units.Last).Utype := Is_Spec_Only;
         end if;
      end if;

      --  Scan out external version references and put in hash table

      E_Loop : loop
         Check_Unknown_Line;
         exit E_Loop when C /= 'E';

         if Ignore ('E') then
            Skip_Line;

         else
            Checkc (' ');
            Skip_Space;

            Name_Len := 0;
            Name_Len := 0;
            loop
               C := Getc;

               if C < ' ' then
                  Fatal_Error;
               end if;

               exit when At_End_Of_Field;
               Add_Char_To_Name_Buffer (C);
            end loop;

            Version_Ref.Set (new String'(Name_Buffer (1 .. Name_Len)), True);
            Skip_Eol;
         end if;

         C := Getc;
      end loop E_Loop;

      --  Scan out source dependency lines for this ALI file

      ALIs.Table (Id).First_Sdep := Sdep.Last + 1;

      D_Loop : loop
         Check_Unknown_Line;
         exit D_Loop when C /= 'D';

         if Ignore ('D') then
            Skip_Line;

         else
            Checkc (' ');
            Skip_Space;
            Sdep.Increment_Last;

            --  The file/path name may be quoted

            Sdep.Table (Sdep.Last).Sfile :=
              Get_File_Name (Lower => True, May_Be_Quoted => True);

            Sdep.Table (Sdep.Last).Stamp := Get_Stamp;
            Sdep.Table (Sdep.Last).Dummy_Entry :=
              (Sdep.Table (Sdep.Last).Stamp = Dummy_Time_Stamp);

            --  Acquire checksum value

            Skip_Space;

            declare
               Ctr : Natural;
               Chk : Word;

            begin
               Ctr := 0;
               Chk := 0;

               loop
                  exit when At_Eol or else Ctr = 8;

                  if Nextc in '0' .. '9' then
                     Chk := Chk * 16 +
                              Character'Pos (Nextc) - Character'Pos ('0');

                  elsif Nextc in 'a' .. 'f' then
                     Chk := Chk * 16 +
                              Character'Pos (Nextc) - Character'Pos ('a') + 10;

                  else
                     exit;
                  end if;

                  Ctr := Ctr + 1;
                  P := P + 1;
               end loop;

               if Ctr = 8 and then At_End_Of_Field then
                  Sdep.Table (Sdep.Last).Checksum := Chk;
               else
                  Fatal_Error;
               end if;
            end;

            --  Acquire (sub)unit and reference file name entries

            Sdep.Table (Sdep.Last).Subunit_Name := No_Name;
            Sdep.Table (Sdep.Last).Unit_Name    := No_Name;
            Sdep.Table (Sdep.Last).Rfile        :=
              Sdep.Table (Sdep.Last).Sfile;
            Sdep.Table (Sdep.Last).Start_Line   := 1;

            if not At_Eol then
               Skip_Space;

               --  Here for (sub)unit name

               if Nextc not in '0' .. '9' then
                  Name_Len := 0;
                  while not At_End_Of_Field loop
                     Add_Char_To_Name_Buffer (Getc);
                  end loop;

                  --  Set the (sub)unit name. Note that we use Name_Find rather
                  --  than Name_Enter here as the subunit name may already
                  --  have been put in the name table by the Project Manager.

                  if Name_Len <= 2
                    or else Name_Buffer (Name_Len - 1) /= '%'
                  then
                     Sdep.Table (Sdep.Last).Subunit_Name := Name_Find;
                  else
                     Name_Len := Name_Len - 2;
                     Sdep.Table (Sdep.Last).Unit_Name := Name_Find;
                  end if;

                  Skip_Space;
               end if;

               --  Here for reference file name entry

               if Nextc in '0' .. '9' then
                  Sdep.Table (Sdep.Last).Start_Line := Get_Nat;
                  Checkc (':');

                  Name_Len := 0;

                  while not At_End_Of_Field loop
                     Add_Char_To_Name_Buffer (Getc);
                  end loop;

                  Sdep.Table (Sdep.Last).Rfile := Name_Enter;
               end if;
            end if;

            Skip_Eol;
         end if;

         C := Getc;
      end loop D_Loop;

      ALIs.Table (Id).Last_Sdep := Sdep.Last;

      --  Loop through invocation-graph lines

      G_Loop : loop
         Check_Unknown_Line;
         exit G_Loop when C /= 'G';

         Scan_Invocation_Graph_Line;

         C := Getc;
      end loop G_Loop;

      --  We must at this stage be at an Xref line or the end of file

      if C = EOF then
         return Id;
      end if;

      Check_Unknown_Line;

      if C /= 'X' then
         Fatal_Error;
      end if;

      --  This ALI parser does not care about Xref lines.

      return Id;

   exception
      when Bad_ALI_Format =>
         return No_ALI_Id;
   end Scan_ALI;

   --------------
   -- IS_Scope --
   --------------

   function IS_Scope (IS_Id : Invocation_Signature_Id) return Name_Id is
   begin
      pragma Assert (Present (IS_Id));
      return Invocation_Signatures.Table (IS_Id).Scope;
   end IS_Scope;

   ---------
   -- SEq --
   ---------

   function SEq (F1, F2 : String_Ptr) return Boolean is
   begin
      return F1.all = F2.all;
   end SEq;

   -----------------------------------
   -- Set_Invocation_Graph_Encoding --
   -----------------------------------

   procedure Set_Invocation_Graph_Encoding
     (Kind         : Invocation_Graph_Encoding_Kind;
      Update_Units : Boolean := True)
   is
   begin
      Compile_Time_Invocation_Graph_Encoding := Kind;

      --  Update the invocation-graph encoding of the current unit only when
      --  requested by the caller.

      if Update_Units then
         declare
            Curr_Unit : Unit_Record renames Units.Table (Units.Last);
            Curr_ALI  : ALIs_Record renames ALIs.Table  (Curr_Unit.My_ALI);

         begin
            Curr_ALI.Invocation_Graph_Encoding := Kind;
         end;
      end if;
   end Set_Invocation_Graph_Encoding;

   -----------
   -- SHash --
   -----------

   function SHash (S : String_Ptr) return Vindex is
      H : Word;

   begin
      H := 0;
      for J in S.all'Range loop
         H := H * 2 + Character'Pos (S (J));
      end loop;

      return Vindex (Vindex'First + Vindex (H mod Vindex'Range_Length));
   end SHash;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (IC_Id : Invocation_Construct_Id) return Invocation_Signature_Id
   is
   begin
      pragma Assert (Present (IC_Id));
      return Invocation_Constructs.Table (IC_Id).Signature;
   end Signature;

   --------------------
   -- Spec_Placement --
   --------------------

   function Spec_Placement
     (IC_Id : Invocation_Construct_Id) return Declaration_Placement_Kind
   is
   begin
      pragma Assert (Present (IC_Id));
      return Invocation_Constructs.Table (IC_Id).Spec_Placement;
   end Spec_Placement;

   ------------
   -- Target --
   ------------

   function Target
     (IR_Id : Invocation_Relation_Id) return Invocation_Signature_Id
   is
   begin
      pragma Assert (Present (IR_Id));
      return Invocation_Relations.Table (IR_Id).Target;
   end Target;

end ALI;
