-- { dg-do compile }

package Machine_Attribute is

  type R is null record;
  pragma Machine_Attribute (R, "may_alias");

end Machine_Attribute;
