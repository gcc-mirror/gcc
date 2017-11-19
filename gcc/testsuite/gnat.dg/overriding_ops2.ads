with Overriding_Ops2_Pkg.High;

package Overriding_Ops2 is
   type Consumer is tagged limited private;
private
   type Consumer is
      limited
      new Overriding_Ops2_Pkg.High.High_Level_Session
   with null record;

   overriding procedure Finalize (Self : in out Consumer);
end Overriding_Ops2;
