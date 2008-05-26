with Ada.Finalization;
package Pak is
   type T is new Ada.Finalization.Controlled with null record;
   procedure Initialize (X : in out T);
   procedure Finalize (X : in out T);
   procedure Assign (X : out T'Class);
end Pak;
