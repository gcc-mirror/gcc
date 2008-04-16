limited with Incomplete1;
package Incomplete2 is
   pragma Elaborate_Body;
   generic
      type T is private;
   package G is end G;
   package I1 is new G (Incomplete1.T); -- { dg-error "premature use" }
end Incomplete2;
