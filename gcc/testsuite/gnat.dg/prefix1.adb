package body prefix1 is
   Counter : Integer := 2;
   Table : Arr := (2, 4, 8, 16, 32, 64, 128, 256, 512, 1024);
   function Func (Object : T) return Arr is
   begin
      return Table;
   end; 
end prefix1;
