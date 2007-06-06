package discr1 is

  type R is (One, Two);

  type C_Type (Kind : R) is
  record
    case Kind is
      when One =>
        Name       : Integer;
      when Two =>
        Designator : String (1 .. 40);
    end case;
  end record;
  
  for C_Type use record
    Name        at   0 range 0.. 31;
    Designator  at   0 range 0..319;
    Kind        at  40 range 0..  7;
  end record;
  
  for C_Type'Size use 44 * 8;
  
  procedure Assign (Id : String);

end discr1;
