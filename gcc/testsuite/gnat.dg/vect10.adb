-- { dg-do compile }
-- { dg-options "-w" }

package body Vect10 is

   procedure Add_Mul (X : in out Unit; Y, Z : in Unit) is
   begin
       X := X + Y * Z;
   end;
   pragma Inline_Always (Add_Mul);

   procedure Proc
     (F           : in Rec_Vector;
      First_Index : in Natural;
      Last_Index  : in Natural;
      Result      : out Unit)
   is
   begin
      Result := (others => 0.0);

      for I in First_Index + 1 .. Last_Index loop
         declare
            Local : Rec renames F (I);
         begin
            Add_Mul (Result, Local.Val, Local.Val);
         end;
      end loop;
   end;

end Vect10;
