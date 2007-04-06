package prefix1 is
   type Arr is array (1..10) of Natural;
   type T is tagged null record;
   function Func (Object : T) return Arr;
end prefix1;
