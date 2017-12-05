--  { dg-do compile }

procedure Private_Overriding is

   package Foo is

      type Bar is abstract tagged null record;
   
      procedure Overloaded_Subprogram
         (Self : in out Bar)
         is abstract;
   
      procedure Overloaded_Subprogram
         (Self : in out Bar;
          P1 : Integer)
         is abstract;

      procedure Not_Overloaded_Subprogram
         (Self : in out Bar)
         is abstract;


      type Baz is new Bar with null record;
         -- promise to override both overloaded subprograms,
         -- shouldn't matter that they're defined in the private part,

   private -- workaround: override in the public view

      overriding
      procedure Overloaded_Subprogram
         (Self : in out Baz) 
         is null;

      overriding
      procedure Overloaded_Subprogram
         (Self : in out Baz;
          P1 : Integer) 
          is null;

      overriding
      procedure Not_Overloaded_Subprogram
         (Self : in out Baz)
         is null;

   end Foo;

   Qux : Foo.Baz;
begin

  -- this is allowed, as expected
  Foo.Not_Overloaded_Subprogram(Qux);
  Foo.Overloaded_Subprogram(Qux);
  Foo.Overloaded_Subprogram(Foo.Baz'Class(Qux));
  Foo.Overloaded_Subprogram(Foo.Bar'Class(Qux));

  -- however, using object-dot notation
  Qux.Not_Overloaded_Subprogram; -- this is allowed
  Qux.Overloaded_Subprogram; -- "no selector..."
  Foo.Baz'Class(Qux).Overloaded_Subprogram; -- "no selector..."
  Foo.Bar'Class(Qux).Overloaded_Subprogram; -- this is allowed

end Private_Overriding;
