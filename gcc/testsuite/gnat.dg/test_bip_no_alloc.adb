--  { dg-do compile }

pragma Restrictions (No_Allocators);
procedure Test_BIP_No_Alloc is

   type LR (B : Boolean) is limited record
      X : Integer;
   end record;

   function FLR return LR is
   begin
      --  A return statement in a function with a limited and unconstrained
      --  result subtype can result in expansion of an allocator for the
      --  secondary stack, but that should not result in a violation of the
      --  restriction No_Allocators.

      return (B => False, X => 123);
   end FLR;

   Obj : LR := FLR;

begin
   null;
end Test_BIP_No_Alloc;
