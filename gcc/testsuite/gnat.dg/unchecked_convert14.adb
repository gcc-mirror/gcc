--  { dg-do compile }

with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

procedure Unchecked_Convert14 is

    type R is record
       I : Integer;
       C : Character;
    end record;

    subtype Buffer is Storage_Array (1 .. 0);

    function As_Buffer is new Ada.Unchecked_Conversion  --  { dg-warning "types for unchecked conversion have different sizes" }
      (Source => R, Target => Buffer);

    type Buffer_1 is array (Storage_Offset range 1 .. 1) of Storage_Element;

    function As_Buffer_1 is new Ada.Unchecked_Conversion  --  { dg-warning "types for unchecked conversion have different sizes" }
      (Source => R, Target => Buffer_1);

    B : Buffer;
    B_1 : Buffer_1;
    My_R : R := (1, 'x');

begin
   B := As_Buffer (My_R);
   B_1 := As_Buffer_1 (My_R);
end Unchecked_Convert14;
