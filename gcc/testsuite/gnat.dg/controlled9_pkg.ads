with Ada.Finalization;
package Controlled9_Pkg is
   type T is new Ada.Finalization.Controlled with null record;
   type T_Access is access all T;
end Controlled9_Pkg;
