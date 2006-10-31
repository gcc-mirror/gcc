-- { dg-do compile }

procedure Prot1 is
   protected type Prot is
      procedure Change (x : integer);
   private
      Flag : Boolean;
   end Prot;
   type Handle is access protected procedure (X : Integer);
   procedure Manage (Ptr : Handle) is
   begin
      null;
   end;

   protected body prot is
      procedure Change (x : integer) is begin null; end;
   end;

   Sema : Prot;
begin
   Manage (Sema.Change'Unrestricted_Access);
end;
