with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Thunk2_Pkg; use Thunk2_Pkg;

package Thunk2 is

  type Ext is new Root and I with record
    S : Unbounded_String;
  end record;

  overriding function Element (Self : Ext; Name : String) return Ext;

end Thunk2;
