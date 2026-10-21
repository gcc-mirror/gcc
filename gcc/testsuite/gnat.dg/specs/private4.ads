-- { dg-do compile }

generic

  type T1 is tagged private;

package Private4 is

  type T2 is new T1 with record
    I : Integer;
    J : Integer;
  end record;

  X : T2 := T2'(2,3); -- { dg-error "private ancestor|extension aggregate" }
  Y : T2 := T2'(T1 with 2,3);

end Private4;
