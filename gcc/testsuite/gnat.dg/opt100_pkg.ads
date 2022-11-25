with Interfaces; use Interfaces;

package Opt100_Pkg is

  A : constant Unsigned_8 := 0;
  B : constant Unsigned_8 := 1;
  C : constant Unsigned_8 := 2;

  subtype Small_Unsigned_8 is Unsigned_8 range A .. C;

  type Rec is record
    K : Unsigned_8;
    N : Natural;
  end record;

  subtype Small_Rec is Rec
    with Dynamic_Predicate =>
      Small_Rec.K in Small_Unsigned_8 and
        ((Small_Rec.N in Positive) = (Small_Rec.K in B | C));

   function Func (R : Rec) return Integer;

end Opt100_Pkg;
