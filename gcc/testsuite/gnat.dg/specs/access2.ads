--  { dg-do compile }

package Access2 is

    type Priv;
    type Inc is access Priv;
    type Priv is access Inc;
    C : constant Priv := new Inc;

end Access2;
