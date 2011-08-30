-- { dg-do compile }

package aggr1 is
   type Buffer_Array is array (1 .. 2 ** 23) of Integer;
   type Message is record
      Data : Buffer_Array := (others => 0);
   end record;
end;
