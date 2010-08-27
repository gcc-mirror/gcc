package body Dse_Step is

   procedure Do_Step (This : in out Counter) is
   begin
      This.Value := This.Value + This.Step;
   end;

   procedure Step_From (Start : in My_Counter) is
      Lc : My_Counter := Start;
   begin
      while Nsteps > 0 loop
         Do_Step (Lc);
         Nsteps := Nsteps - 1;
      end loop;
      Mv := Lc.Value;
   end;

end;
