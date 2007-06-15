-- { dg-do compile }

package tag1 is
   type T is tagged limited record
      Y : access T'Class;    --  OK
      X : access Tag1.T'Class;  --  Problem
   end record;
end tag1;
