-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure discr_range_check is
   Default_First_Entry : constant := 1;

   task type Server_T (First_Entry : Positive := Default_First_Entry) is
      entry E (First_Entry .. First_Entry);
   end Server_T;

   task body Server_T is begin null; end;

   type Server_Access is access Server_T;
   Server : Server_Access;

begin   
   Server := new Server_T;
end;    
