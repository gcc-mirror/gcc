-- { dg-do run }

procedure Access_Test is

   type T1 is tagged null record;

   procedure Proc_1 (P : access T1'Class)  is
      type Ref is access T1'Class;
      X : Ref := new T1'Class'(P.all);  -- Should always work (no exception)

   begin
      null;
   end;

   procedure Proc_2 is
      type T2 is new T1 with null record;
      X2 : aliased T2;

   begin
      Proc_1 (X2'access);

      declare
         type T3 is new T1 with null record;
         X3 :  aliased T3;

      begin
         Proc_1 (X3'access);
      end;
   end;

begin
   Proc_2;
end;
