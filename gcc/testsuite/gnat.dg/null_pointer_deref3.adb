-- { dg-do run }

-- This test requires architecture- and OS-specific support code for unwinding
-- through signal frames (typically located in *-unwind.h) to pass.  Feel free
-- to disable it if this code hasn't been implemented yet.

procedure Null_Pointer_Deref3 is

   pragma Suppress (All_Checks);

   procedure Leaf is
      type Int_Ptr is access all Integer;
      function n return Int_Ptr is
      begin return null; end;

      Data : Int_Ptr := n;
   begin
      Data.all := 0;
   end;

begin
   Leaf;
exception
   when others => null;
end;
