-- { dg-do compile }

package Weak1 is

   Myconst : constant Integer := 1234;
   pragma Export (C, Myconst, "myconst");
   pragma Weak_External (Myconst);

end Weak1;
