-- { dg-do compile }

package body Renaming11 is

   function F (Arg: Ptr3) return Integer is
      V : Ptr1 renames Arg.all.all;
      I : Integer renames V.A(1);
   begin
      return I;
   end;

end Renaming11;
