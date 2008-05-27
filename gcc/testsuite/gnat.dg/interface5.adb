--  { dg-do compile }
package body interface5 is
   function F (Object : Child) return access Child is
   begin
      return null;
   end F;
end interface5;
