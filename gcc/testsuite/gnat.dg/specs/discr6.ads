-- { dg-do compile }

package Discr6 is

  subtype Index_T is Integer range 0 .. 15;

  type Arr is array (Index_T range <> ) of Long_Long_Integer;

  type Rec2 (Size : Index_T := 2) is record
    A : Arr (2 .. Size);
  end record;

  type Rec3 (D : Boolean := False) is record
    R : Rec2;
    case D is
      when False=> null;
      when True => I : Integer;
    end case;
  end record;

end Discr6;
