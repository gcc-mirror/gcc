--  { dg-do compile }

procedure Fixedpnt8 is

   Ct_A : constant := 0.000_000_100;
   Ct_B : constant := 0.000_000_025;

   Ct_C : constant := 1_000;

   type Number_Type is range 0 .. Ct_C;

   subtype Index_Type is Number_Type range 1 .. Number_Type'Last;

   type Kind_Enumerated_Type is
      (A1,
       A2);

   Kind : Kind_Enumerated_Type := A1;

   V : Duration := 10.0;

   Last : constant Index_Type :=
      Index_Type (V / (case Kind is --  { dg-warning "universal_fixed expression interpreted as type \"Standard.Duration\"" }
                          when A1 => Ct_B,
                          when A2 => Ct_A));
begin
   null;
end Fixedpnt8;