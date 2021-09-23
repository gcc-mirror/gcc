package Opt5_Pkg is

  type Object is tagged private;

  Undefined : constant Object;

  function Is_Defined (Self : Object) return Boolean;

private

  type Object is tagged null record;

  Undefined : constant Object := (others => <>);

  function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

end Opt5_Pkg;
