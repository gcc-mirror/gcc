with Limited_With8_Pkg1;

package body Limited_With8_Pkg2 is
   procedure G (Container : in M;
                F         : access function return Limited_With8_Pkg1.T) is
      Item : Limited_With8_Pkg1.T := F.all;
   begin
      null;
   end G;
end Limited_With8_Pkg2;
