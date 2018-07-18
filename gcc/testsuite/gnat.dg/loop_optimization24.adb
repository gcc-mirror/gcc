-- { dg-do run }
-- { dg-options "-O" }

procedure Loop_Optimization24 is

   procedure Callback is
   begin
      raise Constraint_Error;
   end;

   type Thread_Name_Ptr is access constant String;
   type Callback_Ptr is access procedure;

   type Callback_Information is record
      Name : Thread_Name_Ptr;
      Proc : Callback_Ptr;
   end record;
      
   type Callback_List is array (Positive range <>) of Callback_Information;

   Cbs : Callback_List
     := (1 => (Proc => Callback'access, name => new String'("Callback")),
         2 => (Proc => Callback'access, name => new String'("Callback")));

begin
   for Index in Cbs'Range loop
      begin
         if Cbs(Index).proc /= null then
            Cbs(Index).proc.all;
         end if;
      exception
         when Constraint_Error => null;
      end;
   end loop;
end;
