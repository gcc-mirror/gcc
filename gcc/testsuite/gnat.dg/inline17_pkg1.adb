with Inline17_Pkg2; use Inline17_Pkg2;

package body Inline17_Pkg1 is

   procedure Test is
   begin
      null;
   end;

   function Get (Field : SQL_Field) return Integer is
   begin
      return +Field;
   end;

end Inline17_Pkg1;
