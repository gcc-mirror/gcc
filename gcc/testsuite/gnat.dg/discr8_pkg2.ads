with Discr8_Pkg3; use Discr8_Pkg3;

package Discr8_Pkg2 is

  Max : constant Natural := Value;

  type List_T is array (Natural range <>) of Integer;
  
  type L is record
    List : List_T (1 .. Max);
  end record;
  
end Discr8_Pkg2;
