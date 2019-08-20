package Tagged5 is

    type T is limited interface;

    not overriding function Element
      (Self  : T;
       Index : Positive)
       return Integer is abstract
       with Pre'Class => Index + Index ** 2 in 1 .. 10;

    function First
      (Self  : T'Class)
       return Integer
         is (Self.Element (1));

    procedure Dummy;

end Tagged5;
