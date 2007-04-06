-- { dg-do compile }
-- { dg-options "-gnatws" }

package body volatile2 is
  
  procedure Copy is
    R : Result;
    M : Integer;
    subtype Get_Data is Command_Data (Get, R.Data'Last);       
  begin                                                        
    declare
      G : Get_Data;
      for G'Address use M'Address;
    begin
      for I in 1 .. R.Data'Last loop
        G.Data (I) := (Time => R.Data (I).Time);
      end loop;
    end;
  end;

end volatile2;

