--  { dg-do compile }
--  { dg-options "-gnatws" }

with System.Storage_Pools.Subpools; use System.Storage_Pools.Subpools;
with Ada.Finalization;              use Ada.Finalization;

procedure Allocator4 is

   type My_Subpool_Type is new Root_Subpool with null record;
   type My_Subpool_Access_Type is access all My_Subpool_Type;

   My_Subpool        : aliased My_Subpool_Type;
   My_Subpool_Access : Subpool_Handle := My_Subpool'Unchecked_Access;

   type T is new Ada.Finalization.Controlled with null record;

   A : access T := new (Subpool_Handle'(My_Subpool'Unchecked_Access)) T;
   B : access T := new (My_Subpool_Access) T;
   C : access T := new (My_Subpool'Access) T;

begin
   null;
end;
