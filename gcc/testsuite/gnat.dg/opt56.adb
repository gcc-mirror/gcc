-- { dg-do compile }
-- { dg-options "-O3" }

package body Opt56 is

   function F (Values : Vector) return Boolean is
      Result : Boolean := True;
   begin
      for I in Values'Range loop
         Result := Result and Values (I) >= 0.0;
     end loop;
     return Result;
   end;

end Opt56;
