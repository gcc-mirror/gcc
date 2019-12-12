package Warn28 is

   procedure TheProcedure1 (TheParameter : in Integer);
   procedure Junk (It : Integer);

   generic
   procedure TheProcedure (TheParameter : in Integer);

end Warn28;
