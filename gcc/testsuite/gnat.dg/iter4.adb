--  { dg-do compile }

procedure Iter4 is
   package Root is
      type Result is tagged record
         B : Boolean;
      end record;

      type T is tagged record
         I : Integer;
      end record
      with Iterable => (First       => Pkg.First, --  { dg-error "primitive operation for Iterable type must appear in the same list of declarations as the type" }
                        Next        => Pkg.Next,
                        Has_Element => Pkg.Has_Element,
                        Element     => Pkg.Element);

      package Pkg is
         function First (Dummy : T) return Natural is (0);
         function Next (Dummy : T; Cursor : Natural) return Natural is
           (Cursor + 1);
         function Has_Element (Value : T; Cursor : Natural) return Boolean is
           (Cursor <= Value.I);
         function Element (Dummy : T; Cursor : Natural) return Result is
           ((B => Cursor mod 2 = 0));
      end Pkg;
   end Root;

   package Derived is
      type T is new Root.T with record
         C : Character;
      end record;
   end Derived;

begin
   null;
end;
