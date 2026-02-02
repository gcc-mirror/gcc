limited with Limited_With8_Pkg1;

package Limited_With8_Pkg2 is
   type M is tagged null record;
   procedure G (Container : in M;
                F         : access function return Limited_With8_Pkg1.T);
end Limited_With8_Pkg2;
