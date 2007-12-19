-- { dg-do compile }

procedure access_discr is
   
   type One;
   
   type Iface is limited interface;
   type Base  is tagged limited null record;
   
   type Two_Alone (Parent : access One) is limited null record;
   type Two_Iface (Parent : access One) is limited new Iface with null record;
   type Two_Base (Parent : access One) is new Base with null record;
   
   type One is limited record
      TA : Two_Alone (One'Access);
      TI : Two_Iface (One'Access); --  OFFENDING LINE
      TB : Two_Base (One'Access);
   end record;

begin
   null;
end;
