-- { dg-do compile }
-- { dg-options "-O2 -fstack-usage" }

with System;

procedure Stack_Usage2 is

   Sink : System.Address;
   pragma Import (Ada, Sink);

   procedure Transmit_Data (Branch : Integer) is
      pragma No_Inline (Transmit_Data);
      X : Integer;
   begin
      case Branch is
         when 1 => Sink := X'Address;
         when others => null;
      end case;
   end;

begin
   Transmit_Data (Branch => 1);
end;

-- { dg-final { scan-stack-usage-not ":Constprop" } }
-- { dg-final { cleanup-stack-usage } }
