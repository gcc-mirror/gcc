with Generic_Inst2_C;

package Generic_Inst2 is
   type T is private;
   procedure Foo (X : not null access T);
   package CI is new Generic_Inst2_C (T, Foo => Foo);
private
   type S is access Integer;
   type T is new S;
end;
