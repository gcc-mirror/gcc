-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

package body Lto4 is

   procedure SS_Allocate (Stack : Stack_Ptr) is
      Chunk : Chunk_Ptr := Stack.Current_Chunk;
   begin
      Chunk := new Chunk_Id (First => Chunk.Last, Last  => Chunk.Last);
   end;

end Lto4;
