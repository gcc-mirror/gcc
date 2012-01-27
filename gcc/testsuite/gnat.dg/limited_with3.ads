with Limited_With3_Pkg1;
with Limited_With3_Pkg2;
limited with Limited_With3_Pkg3;

package Limited_With3 is

     procedure Dummy;

     type T is tagged private;

private

     package My_Q is new Limited_With3_Pkg1 (Limited_With3_Pkg2.T);

     type T is tagged null record;

end Limited_With3;
