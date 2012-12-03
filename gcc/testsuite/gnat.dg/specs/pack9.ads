-- { dg-do compile }

package Pack9 is

  subtype Zero is Natural range 0 .. 0;

  type Rec (D : Boolean) is record
    case D is
       when True => Z : Zero;
       when False => null;
    end case;
  end record;
  pragma Pack (Rec);
    
end Pack9;
