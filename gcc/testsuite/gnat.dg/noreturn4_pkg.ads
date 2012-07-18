with Ada.Finalization; use Ada.Finalization;

package Noreturn4_Pkg is

  type Priv is private;
  function It return Priv;
  function Value (Obj : Priv) return Integer;
  function OK (Obj : Priv) return Boolean;

private
  type Priv is new Controlled with record
     Value : Integer := 15;
  end record;

  procedure Adjust   (Obj : in out Priv);
  procedure Finalize (Obj : in out Priv);

end Noreturn4_Pkg;
