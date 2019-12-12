--  { dg-do compile }

package body Weak3 is

   type T is new Integer;
   pragma Weak_External (T); --  { dg-error "pragma applies to objects and subprograms" }
   X : T;

   procedure Foo is null;

end Weak3;
