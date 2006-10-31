-- { dg-do compile }

procedure loop_bound is
   package P is
      type Base is new Integer;
      Limit : constant Base := 10;
      type Index is private;
      generic package Gen is end;
   private 
      type Index is new Base range 0 .. Limit;
   end P;  
   package body P is
      package body Gen is
         type Table is array (Index) of Integer;
         procedure Init (X : in out Table) is
         begin   
            for I in 1..Index'last -1 loop 
               X (I) := -1;
            end loop;
         end Init;
      end Gen;
   end P;  
   package Inst is new P.Gen;
begin   
   null;   
end;    
