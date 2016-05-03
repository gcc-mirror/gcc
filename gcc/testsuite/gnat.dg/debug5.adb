-- { dg-do compile }
-- { dg-options "-g" }

procedure Debug5 is

   type Record_Type (L1, L2 : Natural) is record
      S1 : String (1 .. L1);
      case L2 is
         when 0      => null;
         when others => S2 : String (L1 .. L2);
      end case;
   end record;

   procedure Discard (R : Record_Type) is
   begin
      null;
   end Discard;

   R : constant Record_Type := (0, 0, others => <>);
begin
   Discard (R);
end Debug5;
