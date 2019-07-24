with Inline17_Pkg3; use Inline17_Pkg3;

package Inline17_Pkg2 is

   subtype SQL_Field is Inline17_Pkg3.SQL_Field;

   function "+" (Field : SQL_Field'Class) return Integer renames
       Inline17_Pkg3."+";

end Inline17_Pkg2;
