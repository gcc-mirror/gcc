with Ada.Streams; use Ada.Streams;

package body Opt41_Pkg is

   type Wstream is new Root_Stream_Type with record
      S : Unbounded_String;
   end record;

   procedure Read (Stream : in out Wstream;
                   Item   : out Stream_Element_Array;
                   Last   : out Stream_Element_Offset) is null;

   procedure Write (Stream : in out Wstream; Item : Stream_Element_Array) is
   begin
      for J in Item'Range loop
         Append (Stream.S, Character'Val (Item (J)));
      end loop;
   end Write;

   function Rec_Write (R : Rec) return Unbounded_String is
      S : aliased Wstream;
   begin
      Rec'Output (S'Access, R);
      return S.S;
   end Rec_Write;

   type Rstream is new Root_Stream_Type with record
      S   : String_Access;
      Idx : Integer := 1;
   end record;

   procedure Write (Stream : in out Rstream; Item : Stream_Element_Array) is null;

   procedure Read (Stream : in out Rstream;
                   Item   : out Stream_Element_Array;
                   Last   : out Stream_Element_Offset) is
   begin
      Last := Stream_Element_Offset'Min
         (Item'Last, Item'First + Stream_Element_Offset (Stream.S'Last - Stream.Idx));
      for I in Item'First .. Last loop
         Item (I) := Stream_Element (Character'Pos (Stream.S (Stream.Idx)));
         Stream.Idx := Stream.Idx + 1;
      end loop;
   end Read;

   function Rec_Read (Str : String_Access) return Rec is
      S : aliased Rstream;
   begin
      S.S := Str;
      return Rec'Input (S'Access);
   end Rec_Read;

end Opt41_Pkg;
