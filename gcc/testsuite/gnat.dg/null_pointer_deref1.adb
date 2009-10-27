-- { dg-do run }
-- { dg-options "-gnatp" }

-- This test requires architecture- and OS-specific support code for unwinding
-- through signal frames (typically located in *-unwind.h) to pass.  Feel free
-- to disable it if this code hasn't been implemented yet.

procedure Null_Pointer_Deref1 is
   type Int_Ptr is access all Integer;

   function Ident return Int_Ptr is
   begin
     return null;
   end;

   Data : Int_Ptr := Ident;
begin
   Data.all := 1;
exception
   when Constraint_Error | Storage_Error => null;
end;
