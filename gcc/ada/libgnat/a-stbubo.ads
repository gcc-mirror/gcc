------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.STRINGS.TEXT_BUFFERS.BOUNDED                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2024, Free Software Foundation, Inc.       --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Strings.Text_Buffers.Bounded with
   Pure
is

   type Buffer_Type (Max_Characters : Text_Buffer_Count) is
     new Root_Buffer_Type with private with
      Default_Initial_Condition => not Text_Truncated (Buffer_Type);

   function Text_Truncated (Buffer : Buffer_Type) return Boolean;

   function Get (Buffer : in out Buffer_Type) return String with
      Post'Class => Get'Result'First = 1 and then Current_Indent (Buffer) = 0;

   function Wide_Get (Buffer : in out Buffer_Type) return Wide_String with
      Post'Class => Wide_Get'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Wide_Wide_Get
     (Buffer : in out Buffer_Type) return Wide_Wide_String with
      Post'Class => Wide_Wide_Get'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Get_UTF_8
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_8_String with
      Post'Class => Get_UTF_8'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Wide_Get_UTF_16
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_16_Wide_String with
      Post'Class => Wide_Get_UTF_16'Result'First = 1
      and then Current_Indent (Buffer) = 0;

private

   procedure Put_UTF_8_Implementation
     (Buffer : in out Root_Buffer_Type'Class;
      Item   : UTF_Encoding.UTF_8_String)
     with Pre => Buffer in Buffer_Type'Class;

   package Mapping is new Output_Mapping (Put_UTF_8_Implementation);

   subtype Positive_Text_Buffer_Count is
     Text_Buffer_Count range 1 .. Text_Buffer_Count'Last;

   type Convertible_To_UTF_8_String is
     array (Positive_Text_Buffer_Count range <>) of Character;

   type Buffer_Type (Max_Characters : Text_Buffer_Count)
   is new Mapping.Buffer_Type with record
      Truncated : Boolean := False;
      --  True if we ran out of space on a Put

      Chars : Convertible_To_UTF_8_String (1 .. Max_Characters);
   end record;

end Ada.Strings.Text_Buffers.Bounded;
