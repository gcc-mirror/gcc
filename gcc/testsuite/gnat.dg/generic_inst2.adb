--  { dg-do compile }

package body Generic_Inst2 is
   procedure Foo (X : not null access T) is null;
end;
