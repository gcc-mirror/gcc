--  { dg-do run }

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Deallocation;

procedure Allocator_Maxalign1 is

   Max_Alignment : constant := Standard'Maximum_Alignment;

   type Block is record
      X : Integer;
   end record;
   for Block'Alignment use Standard'Maximum_Alignment;

   type Block_Access is access all Block;
   procedure Free is new Ada.Unchecked_Deallocation (Block, Block_Access);

   N_Blocks : constant := 500;
   Blocks   : array (1 .. N_Blocks) of Block_Access;
begin
   if Block'Alignment /= Max_Alignment then
      raise Program_Error;
   end if;

   for K in 1 .. 4 loop

      for I in Blocks'Range loop
         Blocks (I) := new Block;
         if Blocks (I).all'Address mod Block'Alignment /= 0 then
            raise Program_Error;
         end if;
         Blocks(I).all.X := I;
      end loop;

      for I in Blocks'Range loop
         Free (Blocks (I));
      end loop;

   end loop;

end;

