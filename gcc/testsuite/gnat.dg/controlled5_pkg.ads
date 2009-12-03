with Ada.Finalization; use Ada.Finalization;

package Controlled5_Pkg is

   type Root is tagged private;

   type Inner is new Ada.Finalization.Controlled with null record;

   type T_Root_Class is access all Root'Class;

   function Dummy (I : Integer) return Root'Class;

private

   type Root is tagged record
      F2 : Inner;
   end record;

end Controlled5_Pkg;
