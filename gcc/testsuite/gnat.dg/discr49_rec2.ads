with Discr49_Rec1; use Discr49_Rec1;

package Discr49_Rec2 is
   type Child (Discr : Boolean) is private;
   function Value (Obj : Child) return Integer;

private
   type Child (Discr : Boolean) is
     new Parent (Discr_1 => Discr, Discr_2 => True);
end Discr49_Rec2;
