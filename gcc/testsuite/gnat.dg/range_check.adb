-- { dg-do run }

procedure range_check is
   function ident (x : integer) return integer is
   begin   
      return x;
   end ident;

   guard1 : Integer;

   r : array (1 .. ident (10)) of integer;
   pragma Suppress (Index_Check, r);

   guard2 : Integer;

begin
   guard1 := 0;
   guard2 := 0;
   r (11) := 3;
end;
