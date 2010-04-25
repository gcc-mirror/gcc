-- { dg-do compile }

package body Pack15 is

  procedure Transfer is
  begin
    O.Status_Flags := Status_Flags;
  end;

end Pack15;
