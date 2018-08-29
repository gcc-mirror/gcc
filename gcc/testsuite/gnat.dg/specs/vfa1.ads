-- { dg-do compile }
-- { dg-options "-g" }

package VFA1 is

  type Rec is record
    A : Short_Integer;
    B : Short_Integer;
  end record;

  type Rec_VFA is new Rec;
  pragma Volatile_Full_Access (Rec_VFA);

end VFA1;
