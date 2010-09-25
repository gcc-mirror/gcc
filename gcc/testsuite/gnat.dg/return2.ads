package Return2 is

  type Kind_T is (One, Two);

  type T is array (Kind_T) of Boolean;

  type Result_Internal_T (Member : Boolean := False) is record
    case Member is
      when True =>
        Data : Kind_T := Kind_T'First;
      when False =>
        null;
    end case;
  end record;

  function Value (Img : String) return T;

end Return2;
