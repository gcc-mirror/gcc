limited with Limited_With3_Pkg3;

package Limited_With3_Pkg2 is

    type T is tagged null record;

    procedure Proc (X : Limited_With3_Pkg3.TT; Y : T);

end Limited_With3_Pkg2;

