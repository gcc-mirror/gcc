with Ada.Text_Io; use Ada.Text_Io;
with Raise_From_Pure; use Raise_From_Pure;
package body Wrap_Raise_From_Pure is
   procedure Check is
      K : Integer;
   begin
      K := Raise_CE_If_0 (0);
      Put_Line ("Should never reach here");
   end;
end;
