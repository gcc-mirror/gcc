-- { dg-do compile }
-- { dg-options "-O2" }

package body Thunk2 is

  overriding function Element (Self : Ext; Name : String) return Ext is
  begin
    return Self;
  end;

end Thunk2;
