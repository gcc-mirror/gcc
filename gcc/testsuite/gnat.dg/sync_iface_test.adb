--  { dg-do compile }
package body Sync_Iface_Test is
   protected body Buffer is
      procedure Dummy is begin null; end;
   end;

   function First (Obj : Buffer) return Natural is
   begin
     return 0;
   end;

   procedure Do_Test (Dummy : Natural; Item : Buffer)
   is
      Position1 : Natural := First (Item);
      Position2 : Natural := Item.First;   --  Problem here
   begin
      null;
   end;
end;
