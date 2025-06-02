package body SSO19_Pkg is

  function Is_Valid_Private (Item : Data) return Boolean is
  begin
    return Item.I'Valid and Item.F'Valid;
  end Is_Valid_Private;

  function Is_Valid (Item : Rec) return Boolean is
  begin
    return Is_Valid_Private (Item.D);
  end Is_Valid;

end SSO19_Pkg;
