--  { dg-do compile }

procedure Limited5 is

   type Command       is limited null record;
   type Command_Array is array (Positive range <>) of Command;
      
   function To_Commands return Command_Array is
   begin
      return Result : Command_Array (1 .. 2);
   end To_Commands;
   
   The_Commands : aliased Command_Array := To_Commands;

begin
   null;
end;
