-- PR ada/15605
-- { dg-do compile }

package Profile1 is

  subtype Int is Integer;
  function F (Int : Integer) return Int; -- { dg-error "formal parameter" }

  procedure Foo (X : Integer);
  procedure P (Foo : Integer) renames Foo; -- { dg-error "formal parameter" }

end Profile1;
