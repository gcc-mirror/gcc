-- { dg-do compile }

package Static_Initializer4 is

  type R is tagged record
    b : Boolean;
  end record;

  type NR is new R with null record;

  C : NR := (b => True);

end Static_Initializer4;
