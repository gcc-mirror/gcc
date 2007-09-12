--  { dg-do compile }

procedure Discr_Test2 is
   type Ptr is access all integer;
   type Ar is array (Integer range <>) of Ptr;
        
   type Inner (Discr : Integer) is record
      Comp : Ar (1..Discr);
   end record;

   type Wrapper (Discr : Integer) is record
      Comp :  Inner (Discr);
   end record;

   Val  : constant Wrapper := (0, Comp => <>);
begin
   null;
end;
