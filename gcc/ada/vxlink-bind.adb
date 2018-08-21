------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          V X L I N K . B I N D                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

pragma Ada_2012;

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;

with GNAT.Regpat;       use GNAT.Regpat;

package body VxLink.Bind is

   function Split_Lines (S : String) return Strings_List.Vector;

   function Split (S : String; C : Character) return Strings_List.Vector;

   function Parse_Nm_Output (S : String) return Symbol_Sets.Set;

   procedure Emit_Module_Dtor
     (FP : File_Type);

   procedure Emit_CDtor
     (FP  : File_Type;
      Var : String;
      Set : Symbol_Sets.Set);

   -----------------
   -- Split_Lines --
   -----------------

   function Split_Lines (S : String) return Strings_List.Vector
   is
      Last : Natural := S'First;
      Ret  : Strings_List.Vector;
   begin
      for J in S'Range loop
         if S (J) = ASCII.CR
           and then J < S'Last
           and then S (J + 1) = ASCII.LF
         then
            Ret.Append (S (Last .. J - 1));
            Last := J + 2;
         elsif S (J) = ASCII.LF then
            Ret.Append (S (Last .. J - 1));
            Last := J + 1;
         end if;
      end loop;

      if Last <= S'Last then
         Ret.Append (S (Last .. S'Last));
      end if;

      return Ret;
   end Split_Lines;

   -----------
   -- Split --
   -----------

   function Split (S : String; C : Character) return Strings_List.Vector
   is
      Last : Natural := S'First;
      Ret  : Strings_List.Vector;
   begin
      for J in S'Range loop
         if S (J) = C then
            if J > Last then
               Ret.Append (S (Last .. J - 1));
            end if;

            Last := J + 1;
         end if;
      end loop;

      if Last <= S'Last then
         Ret.Append (S (Last .. S'Last));
      end if;

      return Ret;
   end Split;

   ---------------------
   -- Parse_Nm_Output --
   ---------------------

   function Parse_Nm_Output (S : String) return Symbol_Sets.Set
   is
      Nm_Regexp        : constant Pattern_Matcher :=
                           Compile ("^[0-9A-Za-z]* ([a-zA-Z]) (.*)$");
      type CDTor_Type is
        (CTOR_Diab,
         CTOR_Gcc,
         DTOR_Diab,
         DTOR_Gcc);
      subtype CTOR_Type is CDTor_Type range CTOR_Diab .. CTOR_Gcc;
      CTOR_DIAB_Regexp : aliased constant Pattern_Matcher :=
                           Compile ("^__?STI__*([0-9]+)_");
      CTOR_GCC_Regexp  : aliased constant Pattern_Matcher :=
                           Compile ("^__?GLOBAL_.I._*([0-9]+)_");
      DTOR_DIAB_Regexp : aliased constant Pattern_Matcher :=
                           Compile ("^__?STD__*([0-9]+)_");
      DTOR_GCC_Regexp  : aliased constant Pattern_Matcher :=
                           Compile ("^__?GLOBAL_.D._*([0-9]+)_");
      type Regexp_Access is access constant Pattern_Matcher;
      CDTor_Regexps    : constant array (CDTor_Type) of Regexp_Access :=
                           (CTOR_Diab => CTOR_DIAB_Regexp'Access,
                            CTOR_Gcc  => CTOR_GCC_Regexp'Access,
                            DTOR_Diab => DTOR_DIAB_Regexp'Access,
                            DTOR_Gcc  => DTOR_GCC_Regexp'Access);
      Result           : Symbol_Sets.Set;

   begin
      for Line of Split_Lines (S) loop
         declare
            Sym     : Symbol;
            Nm_Grps : Match_Array (0 .. 2);
            Ctor_Grps : Match_Array (0 .. 1);
         begin
            Match (Nm_Regexp, Line, Nm_Grps);

            if Nm_Grps (0) /= No_Match then
               declare
                  Sym_Type : constant Character :=
                               Line (Nm_Grps (1).First);
                  Sym_Name : constant String :=
                               Line (Nm_Grps (2).First .. Nm_Grps (2).Last);
               begin
                  Sym :=
                    (Name     => To_Unbounded_String (Sym_Name),
                     Cat      => Sym_Type,
                     Internal => False,
                     Kind     => Sym_Other,
                     Priority => -1);

                  for J in CDTor_Regexps'Range loop
                     Match (CDTor_Regexps (J).all, Sym_Name, Ctor_Grps);

                     if Ctor_Grps (0) /= No_Match then
                        if J in CTOR_Type then
                           Sym.Kind := Sym_Ctor;
                        else
                           Sym.Kind := Sym_Dtor;
                        end if;

                        Sym.Priority := Integer'Value
                          (Line (Ctor_Grps (1).First .. Ctor_Grps (1).Last));

                        exit;
                     end if;
                  end loop;

                  Result.Include (Sym);
               end;
            end if;
         end;
      end loop;

      return Result;
   end Parse_Nm_Output;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Binder      : out VxLink_Binder;
      Object_File : String)
   is
      Args                   : Arguments_List;
      Module_Dtor_Not_Needed : Boolean := False;
      Module_Dtor_Needed     : Boolean := False;

   begin
      Args.Append (Nm);
      Args.Append (Object_File);

      declare
         Output  : constant String := Run (Args);
         Symbols : Symbol_Sets.Set;
      begin
         if Is_Error_State then
            return;
         end if;

         Symbols := Parse_Nm_Output (Output);

         for Sym of Symbols loop
            if Sym.Kind = Sym_Ctor then
               Binder.Constructors.Insert (Sym);
            elsif Sym.Kind = Sym_Dtor then
               Binder.Destructors.Insert (Sym);
            elsif Match ("_?__.*_atexit$", To_String (Sym.Name)) then
               if Sym.Cat = 'T' then
                  Module_Dtor_Not_Needed := True;
               elsif Sym.Cat = 'U' then
                  Module_Dtor_Needed := True;
               end if;
            end if;
         end loop;

         Binder.Module_Dtor_Needed :=
           not Module_Dtor_Not_Needed and then Module_Dtor_Needed;
      end;
   end Initialize;

   --------------------
   -- Parse_Tag_File --
   --------------------

   procedure Parse_Tag_File
     (Binder : in out VxLink_Binder;
      File   : String)
   is
      FP   : Ada.Text_IO.File_Type;

   begin
      Open
        (FP,
         Mode => In_File,
         Name => File);
      loop
         declare
            Line   : constant String :=
                      Ada.Strings.Fixed.Trim
                        (Get_Line (FP), Ada.Strings.Both);
            Tokens : Strings_List.Vector;

         begin
            if Line'Length = 0 then
               --  Skip empty lines
               null;

            elsif Line (Line'First) = '#' then
               --  Skip comment
               null;

            else
               Tokens := Split (Line, ' ');
               if Tokens.First_Element = "section" then
                  --  Sections are not used for tags, only when building
                  --  kernels. So skip for now
                  null;
               else
                  Binder.Tags_List.Append (Line);
               end if;
            end if;
         end;
      end loop;

   exception
      when Ada.IO_Exceptions.End_Error =>
         Close (FP);
      when others =>
         Log_Error ("Cannot open file " & File &
                      ". DKM tags won't be generated");
   end Parse_Tag_File;

   ----------------------
   -- Emit_Module_Dtor --
   ----------------------

   procedure Emit_Module_Dtor
     (FP : File_Type)
   is
      Dtor_Name : constant String := "_GLOBAL__D_65536_0_cxa_finalize";
   begin
      Put_Line (FP, "extern void __cxa_finalize(void *);");
      Put_Line (FP, "static void " & Dtor_Name & "()");
      Put_Line (FP, "{");
      Put_Line (FP, "  __cxa_finalize(&__dso_handle);");
      Put_Line (FP, "}");
      Put_Line (FP, "");
   end Emit_Module_Dtor;

   ----------------
   -- Emit_CDtor --
   ----------------

   procedure Emit_CDtor
     (FP  : File_Type;
      Var : String;
      Set : Symbol_Sets.Set)
   is
   begin
      for Sym of Set loop
         if not Sym.Internal then
            Put_Line (FP, "extern void " & To_String (Sym.Name) & "();");
         end if;
      end loop;

      New_Line (FP);

      Put_Line (FP, "extern void (*" & Var & "[])();");
      Put_Line (FP, "void (*" & Var & "[])() =");
      Put_Line (FP, "  {");
      for Sym of Set loop
         Put_Line (FP, "  " & To_String (Sym.Name) & ",");
      end loop;
      Put_Line (FP, "  0};");
      New_Line (FP);
   end Emit_CDtor;

   ---------------
   -- Emit_CTDT --
   ---------------

   procedure Emit_CTDT
     (Binder    : in out VxLink_Binder;
      Namespace : String)
   is
      FP         : Ada.Text_IO.File_Type;
      CDtor_File : constant String := Namespace & "-cdtor.c";
   begin
      Binder.CTDT_File := To_Unbounded_String (CDtor_File);
      Create
        (File => FP,
         Name => CDtor_File);
      Put_Line (FP, "#if defined(_HAVE_TOOL_XTORS)");
      Put_Line (FP, "#include <vxWorks.h>");
      if Binder.Module_Dtor_Needed then
         Put_Line (FP, "#define _WRS_NEED_CALL_CXA_FINALIZE");
      end if;
      Put_Line (FP, "#include TOOL_HEADER (toolXtors.h)");
      Put_Line (FP, "#else");
      Put_Line (FP, "");

      if Binder.Module_Dtor_Needed then
         Emit_Module_Dtor (FP);
      end if;

      Emit_CDtor (FP, "_ctors", Binder.Constructors);
      Emit_CDtor (FP, "_dtors", Binder.Destructors);

      Put_Line (FP, "#endif");

      if not Binder.Tags_List.Is_Empty then
         New_Line (FP);
         Put_Line (FP, "/* build variables */");
         Put_Line (FP, "__asm(""  .section \"".wrs_build_vars\"",\""a\"""");");
         for Tag of Binder.Tags_List loop
            Put_Line (FP, "__asm(""  .ascii \""" & Tag & "\"""");");
            Put_Line (FP, "__asm(""  .byte 0"");");
         end loop;
         Put_Line (FP, "__asm(""  .ascii \""end\"""");");
         Put_Line (FP, "__asm(""  .byte 0"");");
      end if;

      Close (FP);

   exception
      when others =>
         Close (FP);
         Set_Error_State ("Internal error");
         raise;
   end Emit_CTDT;

   ---------------
   -- CTDT_File --
   ---------------

   function CTDT_File (Binder : VxLink_Binder) return String
   is
   begin
      return To_String (Binder.CTDT_File);
   end CTDT_File;

end VxLink.Bind;
