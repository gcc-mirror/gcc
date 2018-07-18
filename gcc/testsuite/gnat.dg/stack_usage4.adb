-- { dg-do compile }
-- { dg-options "-Wstack-usage=512" }

with Stack_Usage4_Pkg; use Stack_Usage4_Pkg;

procedure Stack_Usage4 is
   BS : Bounded_String := Get;
   S : String := BS.Data (BS.Data'First .. BS.Len);
begin
   null;
end;
