with Limited_With3;
with Limited_With3_Pkg1;

package Limited_With3_Pkg3 is

    package My_Q is new Limited_With3_Pkg1 (Limited_With3.T);

    type TT is tagged record
       State : My_Q.Element_Access;
    end record;

end Limited_With3_Pkg3;
