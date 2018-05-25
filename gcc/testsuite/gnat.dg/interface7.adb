--  { dg-do compile }

procedure Interface7 is
   type I_Type is interface;

   type A1_Type is tagged null record;
   type A2_Type is new A1_Type and I_Type with null record;

   procedure Test (X : I_Type'Class) is
   begin
      if X in A2_Type then   --  Test
         null;
      end if;
   end Test;

begin null; end;
