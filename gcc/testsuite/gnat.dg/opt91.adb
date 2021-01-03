-- { dg-do compile }
-- { dg-options "-O2 -fchecking=1" }

package body Opt91 is

   function Custom_Image (Self : True_Relation_Rec) return String is
   begin
      return "<True>";
   end;

end Opt91;
