with Lto24_Pkg2;

package Lto24_Pkg1 is

  Max_Elem : constant Natural := Lto24_Pkg2.Get;

  type Arr is array (Natural range <>) of Boolean;

  type Rec (B : Boolean) is record
     I : Integer;
     case B is
       when True => A : Arr (1 .. Max_Elem);
       when False => Empty : Boolean;
     end case;
  end record;

end Lto24_Pkg1;
