--  { dg-do compile }

procedure Warn26 is

   Monitor_Period_Min : constant := 5;
   Monitor_Period_Max : constant := 30;

   type Monitor_Period is range Monitor_Period_Min .. Monitor_Period_Max;

   subtype Period_T is Positive range 5 .. 30;

   function Id (X : Period_T) return Period_T is (X);
   Input_Period : Period_T := Id (20);
begin
   if Input_Period in
      Integer (Monitor_Period'First) .. Integer ( Monitor_Period'Last)
   then
      null;
   end if;
end Warn26;
