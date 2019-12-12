with Interfaces; use type Interfaces.Integer_64;

package Warn30 is
   procedure Incr (X : in out Interfaces.Integer_64) with
     Post => X = X'Old + 1;
end Warn30;
