package Renaming9 is

  pragma Elaborate_Body;

  type Object is tagged null record;

  type Pointer is access all Object'Class;

  type Derived is new Object with record
     I : Integer;
  end record;

  Ptr : Pointer := new Derived;
  Obj : Derived renames Derived (Ptr.all);

end Renaming9;
