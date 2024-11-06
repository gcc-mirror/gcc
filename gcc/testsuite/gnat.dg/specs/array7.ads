-- { dg-do compile }
-- { dg-options "-O" }

package Array7 is

  type I is interface;

  type Rec (Name_Len : Natural) is new I with record
    Input : String (1 .. Name_Len);
  end record;

  function Image (R : Rec) return String is ("I" & String (R.Input));

end Array7;
