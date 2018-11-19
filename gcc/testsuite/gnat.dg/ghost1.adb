--  { dg-do compile }

package body Ghost1 is
   procedure Body_Only (Obj : Ghost_Typ) is null
     with Ghost;

   procedure Spec_And_Body (Obj : Ghost_Typ) is null;
end Ghost1;
