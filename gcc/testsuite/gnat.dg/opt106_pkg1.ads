package Opt106_Pkg1 is

  type T is record
    Initialized   : Boolean;
    Before        : Integer;
    Delayed       : Integer;
    Next          : Integer;
    Attach_Last   : Boolean;
  end record;

  procedure Proc (Obj     : in out T;
                  Data    : Integer;
                  Last    : Boolean;
                  Stretch : Boolean);

end Opt106_Pkg1;
