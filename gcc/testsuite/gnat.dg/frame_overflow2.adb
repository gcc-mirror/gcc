-- { dg-do compile }

with System;

procedure Frame_Overflow2 is -- { dg-error "total size of local objects is too large" }

  type Index_T is range 1 .. 2**(System.Word_Size - 1) - 1;

  type SetArray is array (Index_T) of Boolean;

  type Set is record
    Store: SetArray := (Others => False);
  end record;

  Phi: constant Set := (Store => (Others => False));

  function F return Set is
  begin
    return Phi;
  end;

begin
  null;
end;
