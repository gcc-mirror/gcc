-- { dg-do compile }
-- { dg-options "-g" }

procedure Addr4 is
   function F return String is begin return ""; end F;
   S1 : String renames F;
   subtype ST is String (1 .. S1'Length);
   S2 : ST;
   for S2'Address use S1'Address;
begin
   null;
end;
