-- { dg-do compile }

procedure pointer_array is

   type Node;
   type Node_Ptr is access Node;
   type Node is array (1..10) of Node_Ptr;

   procedure Process (N : Node_Ptr) is
   begin
      null;
   end;

begin
   null;
end;
