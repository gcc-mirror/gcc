with System.Storage_Elements;

package Lto4 is

   package SSE renames System.Storage_Elements;

   type SS_Ptr is new SSE.Integer_Address;

   type Memory is array (SS_Ptr range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;

   type Chunk_Id (First, Last : SS_Ptr) is record
      Mem : Memory (First .. Last);
   end record;

   type Chunk_Ptr is access all Chunk_Id;

   type Stack_Id is record
      Current_Chunk : Chunk_Ptr;
   end record;

   type Stack_Ptr is access Stack_Id;

   procedure SS_Allocate (Stack : Stack_Ptr);

end Lto4;
