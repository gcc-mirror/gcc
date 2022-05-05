-- { dg-do run }

procedure Generic_Comp is

   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
   procedure Gen (Data: in out Array_Type);

   procedure Gen (Data: in out Array_Type) is
   begin
      if not (Data'Length > 1)
        or else not (Integer'(Data'Length) > 1)
        or else not Standard.">" (Data'Length, 1)
        or else not Standard.">" (Integer'(Data'Length), 1)
      then
         raise Program_Error;
      end if;
   end;

   type My_Array is array (Positive range <>) of Integer;

   function Less_Than (L, R : Integer) return Boolean is
   begin
      return L < R;
   end;

   procedure Chk_Down is new Gen (Element_Type => Integer,
                                  Index_Type   => Positive,
                                  Array_Type   => My_Array,
                                  ">"          => Less_Than);

   Data : My_Array (1 .. 2);

begin
   Chk_Down (Data);
end;
