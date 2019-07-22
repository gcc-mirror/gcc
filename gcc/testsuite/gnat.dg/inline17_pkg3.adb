
package body Inline17_Pkg3 is

   function "+" (Field : SQL_Field'Class) return Integer is
   begin
      return 0;
   end;

   function Unchecked_Get (Self : Ref) return Integer is
   begin
      return Self.Data;
   end;

end Inline17_Pkg3;
