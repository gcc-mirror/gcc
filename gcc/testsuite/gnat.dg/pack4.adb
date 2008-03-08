-- { dg-do run }

procedure Pack4 is

   type Time_T is record
      Hour : Integer;
   end record;

   type Date_And_Time_T is record
      Date : Integer;
      Time : Time_T;
   end record;

   pragma Pack(Date_And_Time_T);

   procedure
     Assign_Hour_Of (T : out Time_T)
   is
   begin
      T.Hour := 44;
   end;

   procedure
     Clobber_Hour_Of (DT: out Date_And_Time_T)
   is
   begin
      Assign_Hour_Of (Dt.Time);
   end;

   DT : Date_And_Time_T;

begin
   DT.Time.Hour := 22;
   Clobber_Hour_Of (DT);
   if DT.Time.Hour /= 44 then
     raise Program_Error;
   end if;
end;
