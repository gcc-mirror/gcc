generic

  type User_T is private;

package Deferred_Const4_Pkg is

  type T is private;

  Null_T : constant T;

private

  type T (Valid : Boolean := False) is record
    case Valid is
      when True  => Value : User_T;
      when False => null;
    end case;
  end record;

  Null_T : constant T := (Valid => False);

end Deferred_Const4_Pkg;
