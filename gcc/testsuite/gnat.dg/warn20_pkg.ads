generic
   type T is private;
   None : T;
package Warn20_Pkg is
   generic
      with procedure Dispatch (X : T) is null;
   procedure Foo;
end;
