-- { dg-do compile }

package body Varsize4 is

   function Func (bytes_read : out Natural) return Arr is
      Ret : Arr := (others => False);
   begin
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
