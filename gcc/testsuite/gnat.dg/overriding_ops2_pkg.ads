with Ada.Finalization;

package Overriding_Ops2_Pkg is
   type Session_Type is abstract tagged limited private;
   procedure Finalize (Session : in out Session_Type);
private
   type Session_Type is
     abstract new Ada.Finalization.Limited_Controlled with null record;
end Overriding_Ops2_Pkg;
