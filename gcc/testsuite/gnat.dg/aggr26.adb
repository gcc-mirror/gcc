--  { dg-do compile }

procedure Aggr26 is

    type Row is array (Positive) of Integer;
    H : array (Positive) of Row := (others => (others => 0));  --  { dg-warning "Storage_Error will be raised at run time" }

begin
   null;
end Aggr26;
