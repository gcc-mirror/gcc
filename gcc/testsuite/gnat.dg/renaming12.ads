package Renaming12 is

  type Index_Type is range 0 .. 40;

  type Rec1 is record
    B : Boolean;
  end record;

  type Arr is array (Index_Type range <>) of Rec1;

  type Rec2 (Count : Index_Type := 0) is record
    A : Arr (1 .. Count);
  end record;

  package Ops is

    function "=" (L : Rec2; R : Rec2) return Boolean renames Renaming12."=";

  end Ops;

  procedure Dummy;

end Renaming12;
