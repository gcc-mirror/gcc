------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         R E P I N F O - I N P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2018-2024, Free Software Foundation, Inc.         --
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

with Alloc;
with Csets;    use Csets;
with Hostparm; use Hostparm;
with Namet;    use Namet;
with Output;   use Output;
with Snames;   use Snames;
with Table;
with Ttypes;

package body Repinfo.Input is

   SSU : Pos renames Ttypes.System_Storage_Unit;
   --  Value for Storage_Unit

   type JSON_Entity_Kind is (JE_Record_Type, JE_Array_Type, JE_Other);
   --  Kind of an entity

   type JSON_Entity_Node (Kind : JSON_Entity_Kind := JE_Other) is record
      Esize   : Node_Ref_Or_Val;
      RM_Size : Node_Ref_Or_Val;
      case Kind is
         when JE_Record_Type => Variant        : Nat;
         when JE_Array_Type  => Component_Size : Node_Ref_Or_Val;
         when JE_Other       => Dummy          : Boolean;
      end case;
   end record;
   pragma Unchecked_Union (JSON_Entity_Node);
   --  Record to represent an entity

   package JSON_Entity_Table is new Table.Table (
      Table_Component_Type => JSON_Entity_Node,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Rep_JSON_Table_Initial,
      Table_Increment      => Alloc.Rep_JSON_Table_Increment,
      Table_Name           => "JSON_Entity_Table");
   --  Table of entities

   type JSON_Component_Node is record
      Bit_Offset : Node_Ref_Or_Val;
      Esize      : Node_Ref_Or_Val;
   end record;
   --  Record to represent a component

   package JSON_Component_Table is new Table.Table (
      Table_Component_Type => JSON_Component_Node,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Rep_JSON_Table_Initial,
      Table_Increment      => Alloc.Rep_JSON_Table_Increment,
      Table_Name           => "JSON_Component_Table");
   --  Table of components

   type JSON_Variant_Node is record
      Present : Node_Ref_Or_Val;
      Variant : Nat;
      Next    : Nat;
   end record;
   --  Record to represent a variant

   package JSON_Variant_Table is new Table.Table (
      Table_Component_Type => JSON_Variant_Node,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Rep_JSON_Table_Initial,
      Table_Increment      => Alloc.Rep_JSON_Table_Increment,
      Table_Name           => "JSON_Variant_Table");
   --  Table of variants

   -------------------------------------
   --  Get_JSON_Component_Bit_Offset  --
   -------------------------------------

   function Get_JSON_Component_Bit_Offset
     (Name        : String;
      Record_Name : String) return Node_Ref_Or_Val
   is
      Namid : constant Valid_Name_Id := Name_Find (Record_Name & '.' & Name);
      Index : constant Int := Get_Name_Table_Int (Namid);

   begin
      --  Return No_Uint if no information is available for the component

      if Index = 0 then
         return No_Uint;
      end if;

      return JSON_Component_Table.Table (Index).Bit_Offset;
   end Get_JSON_Component_Bit_Offset;

   -------------------------------
   --  Get_JSON_Component_Size  --
   -------------------------------

   function Get_JSON_Component_Size (Name : String) return Node_Ref_Or_Val is
      Namid : constant Valid_Name_Id := Name_Find (Name);
      Index : constant Int := Get_Name_Table_Int (Namid);

   begin
      --  Return No_Uint if no information is available for the component

      if Index = 0 then
         return No_Uint;
      end if;

      return JSON_Entity_Table.Table (Index).Component_Size;
   end Get_JSON_Component_Size;

   ----------------------
   --  Get_JSON_Esize  --
   ----------------------

   function Get_JSON_Esize (Name : String) return Node_Ref_Or_Val is
      Namid : constant Valid_Name_Id := Name_Find (Name);
      Index : constant Int := Get_Name_Table_Int (Namid);

   begin
      --  Return No_Uint if no information is available for the entity

      if Index = 0 then
         return No_Uint;
      end if;

      return JSON_Entity_Table.Table (Index).Esize;
   end Get_JSON_Esize;

   ----------------------
   --  Get_JSON_Esize  --
   ----------------------

   function Get_JSON_Esize
     (Name        : String;
      Record_Name : String) return Node_Ref_Or_Val
   is
      Namid : constant Valid_Name_Id := Name_Find (Record_Name & '.' & Name);
      Index : constant Int := Get_Name_Table_Int (Namid);

   begin
      --  Return No_Uint if no information is available for the entity

      if Index = 0 then
         return No_Uint;
      end if;

      return JSON_Component_Table.Table (Index).Esize;
   end Get_JSON_Esize;

   ------------------------
   --  Get_JSON_RM_Size  --
   ------------------------

   function Get_JSON_RM_Size (Name : String) return Node_Ref_Or_Val is
      Namid : constant Valid_Name_Id := Name_Find (Name);
      Index : constant Int := Get_Name_Table_Int (Namid);

   begin
      --  Return No_Uint if no information is available for the entity

      if Index = 0 then
         return No_Uint;
      end if;

      return JSON_Entity_Table.Table (Index).RM_Size;
   end Get_JSON_RM_Size;

   -----------------------
   --  Read_JSON_Stream --
   -----------------------

   procedure Read_JSON_Stream (Text : Text_Buffer; File_Name : String) is

      type Text_Position is record
         Index  : Text_Ptr := 0;
         Line   : Natural := 0;
         Column : Natural := 0;
      end record;
      --  Record to represent position in the text

      type Token_Kind is
        (J_NULL,
         J_TRUE,
         J_FALSE,
         J_NUMBER,
         J_INTEGER,
         J_STRING,
         J_ARRAY,
         J_OBJECT,
         J_ARRAY_END,
         J_OBJECT_END,
         J_COMMA,
         J_COLON,
         J_EOF);
      --  JSON token kind. Note that in ECMA 404 there is no notion of integer.
      --  Only numbers are supported. In our implementation we return J_INTEGER
      --  if there is no decimal part in the number. The semantic is that this
      --  is a J_NUMBER token that might be represented as an integer. Special
      --  token J_EOF means that end of stream has been reached.

      function Decode_Integer (Lo, Hi : Text_Ptr) return Uint;
      --  Decode and return the integer in Text (Lo .. Hi)

      function Decode_Name (Lo, Hi : Text_Ptr) return Valid_Name_Id;
      --  Decode and return the name in Text (Lo .. Hi)

      function Decode_Symbol (Lo, Hi : Text_Ptr) return TCode;
      --  Decode and return the expression symbol in Text (Lo .. Hi)

      procedure Error (Msg : String);
      pragma No_Return (Error);
      --  Print an error message and raise an exception

      procedure Read_Entity;
      --  Read an entity

      function Read_Name return Valid_Name_Id;
      --  Read a name

      function Read_Name_With_Prefix return Valid_Name_Id;
      --  Read a name and prepend a prefix

      function Read_Number return Uint;
      --  Read a number

      function Read_Numerical_Expr return Node_Ref_Or_Val;
      --  Read a numerical expression

      procedure Read_Record;
      --  Read a record

      function Read_String return Valid_Name_Id;
      --  Read a string

      procedure Read_Token
        (Kind        : out Token_Kind;
         Token_Start : out Text_Position;
         Token_End   : out Text_Position);
      --  Read a token and return it (this is a standard JSON lexer)

      procedure Read_Token_And_Error
        (TK          : Token_Kind;
         Token_Start : out Text_Position;
         Token_End   : out Text_Position);
      pragma Inline (Read_Token_And_Error);
      --  Read a specified token and error out on failure

      function Read_Variant_Part return Nat;
      --  Read a variant part

      procedure Skip_Value;
      --  Skip a value

      Pos : Text_Position := (Text'First, 1, 1);
      --  The current position in the text buffer

      Name_Buffer : Bounded_String (4 * Max_Name_Length);
      --  The buffer used to build full qualifed names

      Prefix_Len : Natural := 0;
      --  The length of the prefix present in Name_Buffer

      ----------------------
      --  Decode_Integer  --
      ----------------------

      function Decode_Integer (Lo, Hi : Text_Ptr) return Uint is
         Len : constant Nat := Int (Hi) - Int (Lo) + 1;

      begin
         --  Decode up to 9 characters manually, otherwise call into Uint

         if Len < 10 then
            declare
               Val : Int := 0;

            begin
               for J in Lo .. Hi loop
                  Val := Val * 10
                           + Character'Pos (Text (J)) - Character'Pos ('0');
               end loop;
               return UI_From_Int (Val);
            end;

         else
            declare
               Val : Uint := Uint_0;

            begin
               for J in Lo .. Hi loop
                  Val := Val * 10
                           + Character'Pos (Text (J)) - Character'Pos ('0');
               end loop;
               return Val;
            end;
         end if;
      end Decode_Integer;

      -------------------
      --  Decode_Name  --
      -------------------

      function Decode_Name (Lo, Hi : Text_Ptr) return Valid_Name_Id is
      begin
         --  Names are stored in lower case so fold them if need be

         if Is_Upper_Case_Letter (Text (Lo)) then
            declare
               S : String (Integer (Lo) .. Integer (Hi));

            begin
               for J in Lo .. Hi loop
                  S (Integer (J)) := Fold_Lower (Text (J));
               end loop;

               return Name_Find (S);
            end;

         else
            declare
               S : String (Integer (Lo) .. Integer (Hi));
               for S'Address use Text (Lo)'Address;

            begin
               return Name_Find (S);
            end;
         end if;
      end Decode_Name;

      ---------------------
      --  Decode_Symbol  --
      ---------------------

      function Decode_Symbol (Lo, Hi : Text_Ptr) return TCode is

         function Cmp12 (A, B : Character) return Boolean;
         pragma Inline (Cmp12);
         --  Compare Text (Lo + 1 .. Lo + 2) with A & B.

         -------------
         --  Cmp12  --
         -------------

         function Cmp12 (A, B : Character) return Boolean is
         begin
            return Text (Lo + 1) = A and then Text (Lo + 2) = B;
         end Cmp12;

         Len : constant Nat := Int (Hi) - Int (Lo) + 1;

      --  Start of processing for Decode_Symbol

      begin
         case Len is
            when 1 =>
               case Text (Lo) is
                  when '+' =>
                     return Plus_Expr;
                  when '-' =>
                     return Minus_Expr; -- or Negate_Expr
                  when '*' =>
                     return Mult_Expr;
                  when '<' =>
                     return Lt_Expr;
                  when '>' =>
                     return Gt_Expr;
                  when '&' =>
                     return Bit_And_Expr;
                  when '#' =>
                     return Discrim_Val;
                  when others =>
                     null;
               end case;
            when 2 =>
               if Text (Lo) = '/' then
                  case Text (Lo + 1) is
                     when 't' =>
                        return Trunc_Div_Expr;
                     when 'c' =>
                        return Ceil_Div_Expr;
                     when 'f' =>
                        return Floor_Div_Expr;
                     when 'e' =>
                        return Exact_Div_Expr;
                     when others =>
                        null;
                  end case;
               elsif Text (Lo + 1) = '=' then
                  case Text (Lo) is
                     when '<' =>
                        return Le_Expr;
                     when '>' =>
                        return Ge_Expr;
                     when '=' =>
                        return Eq_Expr;
                     when '!' =>
                        return Ne_Expr;
                     when others =>
                        null;
                  end case;
               elsif Text (Lo) = 'o' and then Text (Lo + 1) = 'r' then
                  return Truth_Or_Expr;
               end if;
            when 3 =>
               case Text (Lo) is
                  when '?' =>
                     if Cmp12 ('<', '>') then
                        return Cond_Expr;
                     end if;
                  when 'a' =>
                     if Cmp12 ('b', 's') then
                        return Abs_Expr;
                     elsif Cmp12 ('n', 'd') then
                        return Truth_And_Expr;
                     end if;
                  when 'm' =>
                     if Cmp12 ('a', 'x') then
                        return Max_Expr;
                     elsif Cmp12 ('i', 'n') then
                        return Min_Expr;
                     end if;
                  when 'n' =>
                     if Cmp12 ('o', 't') then
                        return Truth_Not_Expr;
                     end if;
                  when 'x' =>
                     if Cmp12 ('o', 'r') then
                        return Truth_Xor_Expr;
                     end if;
                  when 'v' =>
                     if Cmp12 ('a', 'r') then
                        return Dynamic_Val;
                     end if;
                  when others =>
                     null;
               end case;
            when 4 =>
               if Text (Lo) = 'm'
                 and then Text (Lo + 1) = 'o'
                 and then Text (Lo + 2) = 'd'
               then
                  case Text (Lo + 3) is
                     when 't' =>
                        return Trunc_Mod_Expr;
                     when 'c' =>
                        return Ceil_Mod_Expr;
                     when 'f' =>
                        return Floor_Mod_Expr;
                     when others =>
                        null;
                  end case;
               end if;

               pragma Annotate
                 (CodePeer, Intentional,
                  "condition predetermined", "Error called as defensive code");

            when others =>
               null;
         end case;

         Error ("unknown symbol");
      end Decode_Symbol;

      -----------
      -- Error --
      -----------

      procedure Error (Msg : String) is
         L : constant String := Pos.Line'Img;
         C : constant String := Pos.Column'Img;

      begin
         Set_Standard_Error;
         Write_Eol;
         Write_Str (File_Name);
         Write_Char (':');
         Write_Str (L (L'First + 1 .. L'Last));
         Write_Char (':');
         Write_Str (C (C'First + 1 .. C'Last));
         Write_Char (':');
         Write_Line (Msg);
         raise Invalid_JSON_Stream;
      end Error;

      ------------------
      --  Read_Entity --
      ------------------

      procedure Read_Entity is
         Ent         : JSON_Entity_Node;
         Nam         : Name_Id := No_Name;
         Siz         : Node_Ref_Or_Val;
         Token_Start : Text_Position;
         Token_End   : Text_Position;
         TK          : Token_Kind;

      begin
         Ent.Esize          := No_Uint;
         Ent.RM_Size        := No_Uint;
         Ent.Component_Size := No_Uint;

         --  Read the members as string : value pairs

         loop
            case Read_String is
               when Name_Name =>
                  Nam := Read_Name;
               when Name_Record =>
                  if Nam = No_Name then
                     Error ("name expected");
                  end if;
                  Ent.Variant := 0;
                  Prefix_Len := Natural (Length_Of_Name (Nam));
                  Name_Buffer.Chars (1 .. Prefix_Len) := Get_Name_String (Nam);
                  Read_Record;
               when Name_Variant =>
                  Ent.Variant := Read_Variant_Part;
               when Name_Size =>
                  Siz := Read_Numerical_Expr;
                  Ent.Esize := Siz;
                  Ent.RM_Size := Siz;
               when Name_Object_Size =>
                  Ent.Esize := Read_Numerical_Expr;
               when Name_Value_Size =>
                  Ent.RM_Size := Read_Numerical_Expr;
               when Name_Component_Size =>
                  Ent.Component_Size := Read_Numerical_Expr;
               when others =>
                  Skip_Value;
            end case;

            Read_Token (TK, Token_Start, Token_End);
            if TK = J_OBJECT_END then
               exit;
            elsif TK /= J_COMMA then
               Error ("comma expected");
            end if;
         end loop;

         --  Store the entity into the table

         JSON_Entity_Table.Append (Ent);

         --  Associate the name with the entity

         if Nam = No_Name then
            Error ("name expected");
         end if;

         Set_Name_Table_Int (Nam, JSON_Entity_Table.Last);
      end Read_Entity;

      -----------------
      --  Read_Name  --
      -----------------

      function Read_Name return Valid_Name_Id is
         Token_Start : Text_Position;
         Token_End   : Text_Position;

      begin
         --  Read a single string

         Read_Token_And_Error (J_STRING, Token_Start, Token_End);

         return Decode_Name (Token_Start.Index + 1, Token_End.Index - 1);
      end Read_Name;

      -----------------------------
      --  Read_Name_With_Prefix  --
      -----------------------------

      function Read_Name_With_Prefix return Valid_Name_Id is
         Len         : Natural;
         Lo, Hi      : Text_Ptr;
         Token_Start : Text_Position;
         Token_End   : Text_Position;

      begin
         --  Read a single string

         Read_Token_And_Error (J_STRING, Token_Start, Token_End);
         Lo := Token_Start.Index + 1;
         Hi := Token_End.Index - 1;

         --  Prepare for the concatenation with the prefix

         Len := Integer (Hi) - Integer (Lo) + 1;
         if Prefix_Len + 1 + Len > Name_Buffer.Max_Length then
            Error ("Name buffer too small");
         end if;

         Name_Buffer.Length := Prefix_Len + 1 + Len;
         Name_Buffer.Chars (Prefix_Len + 1) := '.';

         --  Names are stored in lower case so fold them if need be

         if Is_Upper_Case_Letter (Text (Lo)) then
            for J in Lo .. Hi loop
               Name_Buffer.Chars (Prefix_Len + 2 + Integer (J - Lo)) :=
                                                         Fold_Lower (Text (J));
            end loop;

         else
            declare
               S : String (Integer (Lo) .. Integer (Hi));
               for S'Address use Text (Lo)'Address;

            begin
               Name_Buffer.Chars (Prefix_Len + 2 .. Prefix_Len + 1 + Len) := S;
            end;
         end if;

         return Name_Find (Name_Buffer);
      end Read_Name_With_Prefix;

      ------------------
      --  Read_Number --
      ------------------

      function Read_Number return Uint is
         Token_Start : Text_Position;
         Token_End   : Text_Position;

      begin
         --  Only integers are to be expected here

         Read_Token_And_Error (J_INTEGER, Token_Start, Token_End);

         return Decode_Integer (Token_Start.Index, Token_End.Index);
      end Read_Number;

      --------------------------
      --  Read_Numerical_Expr --
      --------------------------

      function Read_Numerical_Expr return Node_Ref_Or_Val is
         Code        : TCode;
         Nop         : Integer;
         Ops         : array (1 .. 3) of Node_Ref_Or_Val;
         TK          : Token_Kind;
         Token_Start : Text_Position;
         Token_End   : Text_Position;

      begin
         --  Read either an integer or an expression

         Read_Token (TK, Token_Start, Token_End);
         if TK = J_INTEGER then
            return Decode_Integer (Token_Start.Index, Token_End.Index);

         elsif TK = J_OBJECT then
            --  Read the code of the expression and decode it

            if Read_String /= Name_Code then
               Error ("name expected");
            end if;

            Read_Token_And_Error (J_STRING, Token_Start, Token_End);
            Code := Decode_Symbol (Token_Start.Index + 1, Token_End.Index - 1);
            Read_Token_And_Error (J_COMMA, Token_Start, Token_End);

            --  Read the array of operands

            if Read_String /= Name_Operands then
               Error ("operands expected");
            end if;

            Read_Token_And_Error (J_ARRAY, Token_Start, Token_End);

            Nop := 0;
            Ops := (others => No_Uint);
            loop
               Nop := Nop + 1;
               Ops (Nop) := Read_Numerical_Expr;
               Read_Token (TK, Token_Start, Token_End);
               if TK = J_ARRAY_END then
                  exit;
               elsif TK /= J_COMMA then
                  Error ("comma expected");
               end if;
            end loop;

            Read_Token_And_Error (J_OBJECT_END, Token_Start, Token_End);

            --  Resolve the ambiguity for '-' now

            if Code = Minus_Expr and then Nop = 1 then
               Code := Negate_Expr;
            end if;

            return Create_Node (Code, Ops (1), Ops (2), Ops (3));

         else
            Error ("numerical expression expected");
         end if;
      end Read_Numerical_Expr;

      -------------------
      --  Read_Record  --
      -------------------

      procedure Read_Record is
         Comp        : JSON_Component_Node;
         First_Bit   : Node_Ref_Or_Val := No_Uint;
         Is_First    : Boolean := True;
         Nam         : Name_Id := No_Name;
         Position    : Node_Ref_Or_Val := No_Uint;
         TK          : Token_Kind;
         Token_Start : Text_Position;
         Token_End   : Text_Position;

      begin
         --  Read a possibly empty array of components

         Read_Token_And_Error (J_ARRAY, Token_Start, Token_End);

         loop
            Read_Token (TK, Token_Start, Token_End);
            if Is_First and then TK = J_ARRAY_END then
               exit;
            elsif TK /= J_OBJECT then
               Error ("object expected");
            end if;

            --  Read the members as string : value pairs

            loop
               case Read_String is
                  when Name_Name =>
                     Nam := Read_Name_With_Prefix;
                  when Name_Discriminant =>
                     Skip_Value;
                  when Name_Position =>
                     Position := Read_Numerical_Expr;
                  when Name_First_Bit =>
                     First_Bit := Read_Number;
                  when Name_Size =>
                     Comp.Esize := Read_Numerical_Expr;
                  when others =>
                     Error ("invalid component");
               end case;

               Read_Token (TK, Token_Start, Token_End);
               if TK = J_OBJECT_END then
                  exit;
               elsif TK /= J_COMMA then
                  Error ("comma expected");
               end if;
            end loop;

            --  Compute Component_Bit_Offset from Position and First_Bit,
            --  either symbolically or literally depending on Position.

            if No (Position) or else No (First_Bit) then
               Error ("bit offset expected");
            end if;

            if Position < Uint_0 then
               declare
                  Bit_Position : constant Node_Ref_Or_Val :=
                          Create_Node (Mult_Expr, Position, UI_From_Int (SSU));
               begin
                  if First_Bit = Uint_0 then
                     Comp.Bit_Offset := Bit_Position;
                  else
                     Comp.Bit_Offset :=
                              Create_Node (Plus_Expr, Bit_Position, First_Bit);
                  end if;
               end;
            else
               Comp.Bit_Offset := Position * SSU + First_Bit;
            end if;

            --  Store the component into the table

            JSON_Component_Table.Append (Comp);

            --  Associate the name with the component

            if Nam = No_Name then
               Error ("name expected");
            end if;

            Set_Name_Table_Int (Nam, JSON_Component_Table.Last);

            Read_Token (TK, Token_Start, Token_End);
            if TK = J_ARRAY_END then
               exit;
            elsif TK /= J_COMMA then
               Error ("comma expected");
            end if;

            Is_First := False;
         end loop;
      end Read_Record;

      ------------------
      --  Read_String --
      ------------------

      function Read_String return Valid_Name_Id is
         Token_Start : Text_Position;
         Token_End   : Text_Position;
         Nam         : Valid_Name_Id;

      begin
         --  Read the string and the following colon

         Read_Token_And_Error (J_STRING, Token_Start, Token_End);
         Nam := Decode_Name (Token_Start.Index + 1, Token_End.Index - 1);
         Read_Token_And_Error (J_COLON, Token_Start, Token_End);

         return Nam;
      end Read_String;

      ------------------
      --  Read_Token  --
      ------------------

      procedure Read_Token
        (Kind        : out Token_Kind;
         Token_Start : out Text_Position;
         Token_End   : out Text_Position)
      is
         procedure Next_Char;
         --  Update Pos to point to next char

         function Is_Whitespace return Boolean;
         pragma Inline (Is_Whitespace);
         --  Return True of current character is a whitespace

         function Is_Structural_Token return Boolean;
         pragma Inline (Is_Structural_Token);
         --  Return True if current character is one of the structural tokens

         function Is_Token_Sep return Boolean;
         pragma Inline (Is_Token_Sep);
         --  Return True if current character is a token separator

         procedure Delimit_Keyword (Kw : String);
         --  Helper function to parse tokens such as null, false and true

         ---------------
         -- Next_Char --
         ---------------

         procedure Next_Char is
         begin
            if Pos.Index > Text'Last then
               Pos.Column := Pos.Column + 1;
            elsif Text (Pos.Index) = ASCII.LF then
               Pos.Column := 1;
               Pos.Line := Pos.Line + 1;
            else
               Pos.Column := Pos.Column + 1;
            end if;
            Pos.Index := Pos.Index + 1;
         end Next_Char;

         -------------------
         -- Is_Whitespace --
         -------------------

         function Is_Whitespace return Boolean is
         begin
            return
              Pos.Index <= Text'Last
                and then
              (Text (Pos.Index) = ASCII.LF
                 or else
               Text (Pos.Index) = ASCII.CR
                 or else
               Text (Pos.Index) = ASCII.HT
                 or else
               Text (Pos.Index) = ' ');
         end Is_Whitespace;

         -------------------------
         -- Is_Structural_Token --
         -------------------------

         function Is_Structural_Token return Boolean is
         begin
            return
              Pos.Index <= Text'Last
                and then
              (Text (Pos.Index) = '['
                 or else
               Text (Pos.Index) = ']'
                 or else
               Text (Pos.Index) = '{'
                 or else
               Text (Pos.Index) = '}'
                 or else
               Text (Pos.Index) = ','
                 or else
               Text (Pos.Index) = ':');
         end Is_Structural_Token;

         ------------------
         -- Is_Token_Sep --
         ------------------

         function Is_Token_Sep return Boolean is
         begin
            return
              Pos.Index > Text'Last
                or else
              Is_Whitespace
                or else
              Is_Structural_Token;
         end Is_Token_Sep;

         ---------------------
         -- Delimit_Keyword --
         ---------------------

         procedure Delimit_Keyword (Kw : String) is
            pragma Unreferenced (Kw);
         begin
            while not Is_Token_Sep loop
               Token_End := Pos;
               Next_Char;
            end loop;
         end Delimit_Keyword;

         CC             : Character;
         Can_Be_Integer : Boolean := True;

      --  Start of processing for Read_Token

      begin
         --  Skip leading whitespaces

         while Is_Whitespace loop
            Next_Char;
         end loop;

         --  Initialize token delimiters

         Token_Start := Pos;
         Token_End   := Pos;

         --  End of stream reached

         if Pos.Index > Text'Last then
            Kind := J_EOF;
            return;
         end if;

         CC := Text (Pos.Index);

         if CC = '[' then
            Next_Char;
            Kind := J_ARRAY;
            return;
         elsif CC = ']' then
            Next_Char;
            Kind := J_ARRAY_END;
            return;
         elsif CC = '{' then
            Next_Char;
            Kind := J_OBJECT;
            return;
         elsif CC = '}' then
            Next_Char;
            Kind := J_OBJECT_END;
            return;
         elsif CC = ',' then
            Next_Char;
            Kind := J_COMMA;
            return;
         elsif CC = ':' then
            Next_Char;
            Kind := J_COLON;
            return;
         elsif CC = 'n' then
            Delimit_Keyword ("null");
            Kind := J_NULL;
            return;
         elsif CC = 'f' then
            Delimit_Keyword ("false");
            Kind := J_FALSE;
            return;
         elsif CC = 't' then
            Delimit_Keyword ("true");
            Kind := J_TRUE;
            return;
         elsif CC = '"' then
            --  We expect a string
            --  Just scan till the end the of the string but do not attempt
            --  to decode it. This means that even if we get a string token
            --  it might not be a valid string from the ECMA 404 point of
            --  view.

            Next_Char;
            while Pos.Index <= Text'Last and then Text (Pos.Index) /= '"' loop
               if Text (Pos.Index) in ASCII.NUL .. ASCII.US then
                  Error ("control character not allowed in string");
               end if;

               if Text (Pos.Index) = '\' then
                  Next_Char;
                  if Pos.Index > Text'Last then
                     Error ("non terminated string token");
                  end if;

                  case Text (Pos.Index) is
                     when 'u' =>
                        for Idx in 1 .. 4 loop
                           Next_Char;
                           if Pos.Index > Text'Last
                             or else (Text (Pos.Index) not in 'a' .. 'f'
                                        and then
                                      Text (Pos.Index) not in 'A' .. 'F'
                                        and then
                                      Text (Pos.Index) not in '0' .. '9')
                           then
                              Error ("invalid unicode escape sequence");
                           end if;
                        end loop;
                     when '\' | '/' | '"' | 'b' | 'f' | 'n' | 'r' | 't' =>
                        null;
                     when others =>
                        Error ("invalid escape sequence");
                  end case;
               end if;
               Next_Char;
            end loop;

            --  No quote found report and error

            if Pos.Index > Text'Last then
               Error ("non terminated string token");
            end if;

            Token_End := Pos;

            --  Go to next char and ensure that this is separator. Indeed
            --  construction such as "string1""string2" are not allowed

            Next_Char;
            if not Is_Token_Sep then
               Error ("invalid syntax");
            end if;
            Kind := J_STRING;
            return;
         elsif CC = '-' or else CC in '0' .. '9' then
            --  We expect a number
            if CC = '-' then
               Next_Char;
            end if;

            if Pos.Index > Text'Last then
               Error ("invalid number");
            end if;

            --  Parse integer part of a number. Superfluous leading zeros are
            --  not allowed.

            if Text (Pos.Index) = '0' then
               Token_End := Pos;
               Next_Char;
            elsif Text (Pos.Index) in '1' .. '9' then
               Token_End := Pos;
               Next_Char;
               while Pos.Index <= Text'Last
                 and then Text (Pos.Index) in '0' .. '9'
               loop
                  Token_End := Pos;
                  Next_Char;
               end loop;
            else
               Error ("invalid number");
            end if;

            if Is_Token_Sep then
               --  Valid integer number

               Kind := J_INTEGER;
               return;
            elsif Text (Pos.Index) /= '.'
              and then Text (Pos.Index) /= 'e'
              and then Text (Pos.Index) /= 'E'
            then
               Error ("invalid number");
            end if;

            --  Check for a fractional part

            if Text (Pos.Index) = '.' then
               Can_Be_Integer := False;
               Token_End := Pos;
               Next_Char;
               if Pos.Index > Text'Last
                 or else Text (Pos.Index) not in '0' .. '9'
               then
                  Error ("invalid number");
               end if;

               while Pos.Index <= Text'Last
                 and then Text (Pos.Index) in '0' .. '9'
               loop
                  Token_End := Pos;
                  Next_Char;
               end loop;

            end if;

            --  Check for exponent part

            if Pos.Index <= Text'Last
              and then (Text (Pos.Index) = 'e' or else Text (Pos.Index) = 'E')
            then
               Token_End := Pos;
               Next_Char;
               if Pos.Index > Text'Last then
                  Error ("invalid number");
               end if;

               if Text (Pos.Index) = '-' then
                  --  Also a few corner cases can lead to an integer, assume
                  --  that the number is not an integer.

                  Can_Be_Integer := False;
               end if;

               if Text (Pos.Index) = '-' or else Text (Pos.Index) = '+' then
                  Next_Char;
               end if;

               if Pos.Index > Text'Last
                 or else Text (Pos.Index) not in '0' .. '9'
               then
                  Error ("invalid number");
               end if;

               while Pos.Index <= Text'Last
                 and then Text (Pos.Index) in '0' .. '9'
               loop
                  Token_End := Pos;
                  Next_Char;
               end loop;
            end if;

            if Is_Token_Sep then
               --  Valid decimal number

               if Can_Be_Integer then
                  Kind := J_INTEGER;
               else
                  Kind := J_NUMBER;
               end if;
               return;
            else
               Error ("invalid number");
            end if;
         elsif CC = EOF then
            Kind := J_EOF;
         else
            Error ("Unexpected character");
         end if;
      end Read_Token;

      ----------------------------
      --  Read_Token_And_Error  --
      ----------------------------

      procedure Read_Token_And_Error
        (TK          : Token_Kind;
         Token_Start : out Text_Position;
         Token_End   : out Text_Position)
      is
         Kind : Token_Kind;

      begin
         --  Read a token and errout out if not of the expected kind

         Read_Token (Kind, Token_Start, Token_End);
         if Kind /= TK then
            Error ("specific token expected");
         end if;
      end Read_Token_And_Error;

      -------------------------
      --  Read_Variant_Part  --
      -------------------------

      function Read_Variant_Part return Nat is
         Next        : Nat := 0;
         TK          : Token_Kind;
         Token_Start : Text_Position;
         Token_End   : Text_Position;
         Var         : JSON_Variant_Node;

      begin
         --  Read a nonempty array of components

         Read_Token_And_Error (J_ARRAY, Token_Start, Token_End);

         loop
            Read_Token_And_Error (J_OBJECT, Token_Start, Token_End);

            Var.Variant := 0;

            --  Read the members as string : value pairs

            loop
               case Read_String is
                  when Name_Present =>
                     Var.Present := Read_Numerical_Expr;
                  when Name_Record =>
                     Read_Record;
                  when Name_Variant =>
                     Var.Variant := Read_Variant_Part;
                  when others =>
                     Error ("invalid variant");
               end case;

               Read_Token (TK, Token_Start, Token_End);
               if TK = J_OBJECT_END then
                  exit;
               elsif TK /= J_COMMA then
                  Error ("comma expected");
               end if;
            end loop;

            --  Chain the variant and store it into the table

            Var.Next := Next;
            JSON_Variant_Table.Append (Var);
            Next := JSON_Variant_Table.Last;

            Read_Token (TK, Token_Start, Token_End);
            if TK = J_ARRAY_END then
               exit;
            elsif TK /= J_COMMA then
               Error ("comma expected");
            end if;
         end loop;

         return Next;
      end Read_Variant_Part;

      ------------------
      --  Skip_Value  --
      ------------------

      procedure Skip_Value is
         Array_Depth  : Natural := 0;
         Object_Depth : Natural := 0;
         TK           : Token_Kind;
         Token_Start  : Text_Position;
         Token_End    : Text_Position;

      begin
         --  Read a value without recursing

         loop
            Read_Token (TK, Token_Start, Token_End);

            case TK is
               when J_STRING | J_INTEGER | J_NUMBER =>
                  null;
               when J_ARRAY =>
                  Array_Depth := Array_Depth + 1;
               when J_ARRAY_END =>
                  Array_Depth := Array_Depth - 1;
               when J_OBJECT =>
                  Object_Depth := Object_Depth + 1;
               when J_OBJECT_END =>
                  Object_Depth := Object_Depth - 1;
               when J_COLON | J_COMMA =>
                  if Array_Depth = 0 and then Object_Depth = 0 then
                     Error ("value expected");
                  end if;
               when others =>
                  Error ("value expected");
            end case;

            exit when Array_Depth = 0 and then Object_Depth = 0;
         end loop;
      end Skip_Value;

      Token_Start : Text_Position;
      Token_End   : Text_Position;
      TK          : Token_Kind;
      Is_First    : Boolean := True;

   --  Start of processing for Read_JSON_Stream

   begin
      --  Read a possibly empty array of entities

      Read_Token_And_Error (J_ARRAY, Token_Start, Token_End);

      loop
         Read_Token (TK, Token_Start, Token_End);
         if Is_First and then TK = J_ARRAY_END then
            exit;
         elsif TK /= J_OBJECT then
            Error ("object expected");
         end if;

         Read_Entity;

         Read_Token (TK, Token_Start, Token_End);
         if TK = J_ARRAY_END then
            exit;
         elsif TK /= J_COMMA then
            Error ("comma expected");
         end if;

         Is_First := False;
      end loop;
   end Read_JSON_Stream;

end Repinfo.Input;
