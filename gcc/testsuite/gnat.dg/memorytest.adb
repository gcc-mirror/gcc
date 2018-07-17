--  { dg-do run }

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure memorytest is

   function malloc (size: size_t) return chars_ptr;
   pragma Import (C, malloc);

   C : chars_ptr;

begin
   --  Allocate a string in C ...
   C := malloc (1000);
   -- ... and free it with the GNAT runtime
   Free (C);

   --  now allocate something completely unrelated and free it
   declare
      A2 : Unbounded_String := To_Unbounded_String ("hello");
   begin
      null;
   end;
end;
