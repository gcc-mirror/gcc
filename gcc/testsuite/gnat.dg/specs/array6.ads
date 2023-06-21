-- { dg-do compile }

package Array6 is 

  type Range_Type is range -10 ..  10;
  type Array_Type is array (Range_Type range <> ) of Short_Short_Integer;

  type Record_Type is record 
    A : Array_Type(-2..4);
  end record ;

  Rec : Record_Type := (A => (others => -1));

end Array6;
