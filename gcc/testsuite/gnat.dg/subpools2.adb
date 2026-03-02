-- { dg-do run }

with System.Storage_Elements;       use System.Storage_Elements;
with System.Storage_Pools.Subpools; use System.Storage_Pools.Subpools;

procedure Subpools2 is

   B : Storage_Array (1 .. 128) with Alignment => Standard'Maximum_Alignment;

   type Pool_T    is new Root_Storage_Pool_With_Subpools with null record;
   type Subpool_T is new Root_Subpool with null record;
   type Rec       is record I : Integer := 0; end record;

   overriding function Create_Subpool
     (Pool : in out Pool_T) return not null Subpool_Handle is
     (raise Constraint_Error);

   overriding procedure Deallocate_Subpool
     (Pool : in out Pool_T; Subpool : in out Subpool_Handle) is null;

   overriding procedure Allocate_From_Subpool
     (Pool                     : in out Pool_T;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count;
      Subpool                  : not null Subpool_Handle) is
   begin
      Storage_Address := B'Address;
   end;

   Pool    : Pool_T;
   Subpool : aliased Subpool_T;
   Handle  : Subpool_Handle := Subpool'Unchecked_Access;

   type Subpool_A is access Rec with Storage_Pool => Pool;
   Ptr : Subpool_A;

begin
   Set_Pool_Of_Subpool (Handle, Pool);
   Ptr := new (Handle) Rec'(I => <>);
end;
