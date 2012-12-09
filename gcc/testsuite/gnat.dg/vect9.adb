-- { dg-do compile }
-- { dg-options "-O -w" }

package body Vect9 is

   function Cmove
     (X        : in Unit;
      Y        : in Unit;
      If_True  : in Unit;
      If_False : in Unit)
      return     Unit
   is
      Res : Unit;
   begin
        for P in Unit'Range loop
         if X (P) >= Y (P) then
            Res (P) := If_True (P);
         else
            Res (P) := If_False (P);
         end if;
      end loop;

      return Res;
   end;
   pragma Inline_Always (Cmove);

   procedure Proc
     (This : in Rec;
      CV   : in Unit_Vector;
      Data : in out Unit_Vector)
   is
   begin
      for Index in Data'Range loop
         Data (Index) := Mul (Zero_Unit, Zero_Unit);
         declare
            Addend : constant Unit
              := Cmove (CV (Index), Zero_Unit, Zero_Unit, Zero_Unit) ;
         begin
            Data (Index) := Data(Index) + Addend;
         end;
         This.Data (Index) := Data (Index);
      end loop;
   end;

end Vect9;
