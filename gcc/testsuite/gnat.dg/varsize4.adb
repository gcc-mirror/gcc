-- { dg-do compile }

package body Varsize4 is

   function Func (Bytes_Read : out Natural) return Arr is
      Ret : Arr := (others => False);
   begin
      Bytes_Read := 0;
      return Ret;
   end;

   function Get return Natural is
      Data  : Arr;
      Bytes : Natural;
   begin
      Data := Func (Bytes);
      return Bytes;
   end;

end Varsize4;
