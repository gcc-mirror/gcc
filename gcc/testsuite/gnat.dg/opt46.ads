package Opt46 is

   type Session_Type is limited private;

   procedure Close (Session : Session_Type);

private

   type Session_Data;
   type Session_Data_Access is access Session_Data;

   type Session_Type is record
      Data : Session_Data_Access;
   end record;

end Opt46;
