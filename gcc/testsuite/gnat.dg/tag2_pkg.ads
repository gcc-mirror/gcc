package Tag2_Pkg is
   type Monitor_Interface is interface;

   type Root is abstract tagged null record;

   type Monitor_Type is abstract new Root
      and Monitor_Interface with null record;

   type Synchronous_Monitor (Size : Positive) is new Monitor_Type with
   record
      Queue : String (1 .. Size);
   end record;

   type Virtual_Integer_Register_Refresher (Size : Positive) is
          new Synchronous_Monitor (Size) with null record;
end;
