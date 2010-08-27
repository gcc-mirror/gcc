package Dse_Step is

   type Counter is record
      Value : Natural;
      Step  : Natural;
   end record;
   pragma Suppress_Initialization (Counter);

   procedure Do_Step (This : in out Counter);
   pragma Inline (Do_Step);

   type My_Counter is new Counter;
   pragma Suppress_Initialization (My_Counter);

   procedure Step_From (Start : in My_Counter);

   Nsteps : Natural := 12;
   Mv : Natural;
end;
