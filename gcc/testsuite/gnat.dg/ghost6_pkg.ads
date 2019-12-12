with Ada.Finalization;

package Ghost6_Pkg with
  Ghost
is
   type T is new Ada.Finalization.Controlled with null record;
end Ghost6_Pkg;
