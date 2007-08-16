
package addr2_p is
   
   type Block is array (1 .. 4) of Integer;
   
   procedure Process (Blk : Block);
   
   B1 : constant Block := Block'((1,2,3,4));
   B2 : constant Block := (1,2,3,4);
end;
