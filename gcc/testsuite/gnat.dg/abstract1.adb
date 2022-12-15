--  { dg-do compile }
--  { dg-options "-gnatws" }

with Ada.Tags.Generic_Dispatching_Constructor;  use Ada.Tags;
package body abstract1 is

   function New_T (Stream : not null access Root_Stream_Type'Class)
      return T'Class is
      function Construct is
         new Generic_Dispatching_Constructor (T, Root_Stream_Type'Class, Input);
      E : constant String := String'Input (Stream);
      I : constant Tag := Internal_Tag (E);

   begin
      return Construct (I, Stream);
   end New_T;

   function Input (Stream : not null access Root_Stream_Type'Class)
     return IT is
   begin
      return O : IT do
        Integer'Read (Stream, O.I);
      end return;
   end Input;

   function Input (Stream : not null access Root_Stream_Type'Class)
      return FT is
   begin
      return O : FT do
        Float'Read (Stream, O.F);
      end return;
   end Input;
end abstract1;
