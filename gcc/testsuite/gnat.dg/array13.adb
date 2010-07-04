-- PR ada/38394
-- Reporter: Michael Völske <michael.voelske@medien.uni-weimar.de>

-- { dg-do assemble }

package body Array13 is

   procedure Foo is
      X, Y : T;
   begin
      null;
   end;

end Array13;
