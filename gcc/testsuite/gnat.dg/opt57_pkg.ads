with System.Storage_Pools; use System.Storage_Pools;

with Ada.Finalization; use Ada.Finalization;

package Opt57_Pkg is

   type GC_Pool is abstract new Root_Storage_Pool with null record;

   type Pinned (Pool : access GC_Pool'Class) is new Controlled with null record;

   procedure Finalize (X : in out Pinned);

end Opt57_Pkg;
