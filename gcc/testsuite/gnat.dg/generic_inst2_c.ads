generic
   type T;
   with procedure Foo (X : not null access T) is null;
   with procedure Bar (X : not null access T) is null;
package Generic_Inst2_C is end;
