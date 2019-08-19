package Generic_Inst7_Types is

   type Index is new Integer range 0 .. 10;

   type Element is record
      I : Integer;
   end record;

   type Element_Array is array (Index range <>) of Element;

   type List (Size : Index := 1) is record
      Arr : Element_Array (1 .. Size);
   end record;

end Generic_Inst7_Types;
