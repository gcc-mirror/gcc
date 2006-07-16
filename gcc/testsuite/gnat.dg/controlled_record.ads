with Ada.Finalization;

package Controlled_Record is

   type Point_T is limited private;
   procedure Assert_Invariants (PA : Point_T);

private

   type Coords_T is array (1 .. 2) of Natural;

   type Point_T is new Ada.Finalization.Controlled with record
      Pos : Coords_T := (0, 0);
   end record;

end Controlled_Record;
