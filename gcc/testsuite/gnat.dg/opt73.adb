-- { dg-do run }
-- { dg-options "-O" }

procedure Opt73 is

   type Terminal_Set_Indexed_By_Non_Terminal is
     array (Natural range <>, Natural  range <>) of Boolean with Pack;

   type Terminal_Set_Per_Non_Terminal
     (Last_Terminal     : Natural;
      Last_Non_Terminal : Natural) is
   record
      Map : Terminal_Set_Indexed_By_Non_Terminal
        (1 .. Last_Non_Terminal, 0 .. Last_Terminal);
   end record;

   Follow : Terminal_Set_Per_Non_Terminal (5, 4);
   Expect : Terminal_Set_Per_Non_Terminal :=
     (5, 4, (1 => (2 => True, others => False),
             others => (others => False)));

   procedure Get_Follow (Value : out Terminal_Set_Per_Non_Terminal) is
   begin
      Value.Map := (others => (others => False));
      Value.Map (1, 2) := True;
      Value.Map (2, 0) := Value.Map (2, 0) or Value.Map (1, 0);
   end;

begin
   Get_Follow (Follow);
   if Follow /= Expect then
      raise Program_Error;
   end if;
end;
