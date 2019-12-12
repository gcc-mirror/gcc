package Tagged2 is
   type Device;

   procedure Get_Parent
     (DeviceX : Device;
      Parent  : out Device);

   type Device is tagged null record;
end Tagged2;
