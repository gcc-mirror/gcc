--  { dg-do compile }

package body Tagged2 is

   procedure Get_Parent
     (DeviceX : Device;
      Parent  : out Device) is null;

end Tagged2;
