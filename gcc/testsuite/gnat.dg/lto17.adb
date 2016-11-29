-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

package body Lto17 is

   function To_Chunk_List(C : Chunk) return Chunk_List is
   begin
      return new Chunk_List_Element'(C.Size, C, null);
   end;

end Lto17;
