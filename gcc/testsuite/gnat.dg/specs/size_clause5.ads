-- { dg-do compile }

package Size_Clause5 is

  type Arr is array (1 .. 16) of Boolean;

  type RRec (D : Boolean) is record
    case D is
      when True =>  I : Integer;
      when False => A : Arr;
    end case;
  end record;
  for RRec'Object_Size use 160;
  for RRec'Value_Size use 160;

end Size_Clause5;
