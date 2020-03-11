-- { dg-do compile }

with System.Storage_Elements;
with System.Storage_Pools.Subpools;

procedure Subpools1 is

   use System.Storage_Pools.Subpools;

   package Local_Pools is

      use System.Storage_Elements;

      type Local_Pool is new Root_Storage_Pool_With_Subpools with null record;

      overriding
      function Create_Subpool (Pool: in out Local_Pool)
                               return not null Subpool_Handle;

      overriding
      procedure Allocate_From_Subpool
        (Pool                    : in out Local_Pool;
         Storage_Address         :    out System.Address;
         Size_In_Storage_Elements: in     Storage_Count;
         Alignment               : in     Storage_Count;
         Subpool                 : in     not null Subpool_Handle);

      overriding
      procedure Deallocate_Subpool
        (Pool   : in out Local_Pool;
         Subpool: in out Subpool_Handle) is null;

   end Local_Pools;

   package body Local_Pools is

      type Local_Subpool is new Root_Subpool with null record;

      Dummy_Subpool: aliased Local_Subpool;

      overriding
      function Create_Subpool (Pool: in out Local_Pool)
                               return not null Subpool_Handle 
      is 
      begin 
         return Result: not null Subpool_Handle 
           := Dummy_Subpool'Unchecked_Access
         do
            Set_Pool_Of_Subpool (Result, Pool);
         end return;
      end;

      overriding
      procedure Allocate_From_Subpool
        (Pool                    : in out Local_Pool;
         Storage_Address         :    out System.Address;
         Size_In_Storage_Elements: in     Storage_Count;
         Alignment               : in     Storage_Count;
         Subpool                 : in     not null Subpool_Handle)
      is
         type Storage_Array_Access is access Storage_Array;

         New_Alloc: Storage_Array_Access
           := new Storage_Array (1 .. Size_In_Storage_Elements + Alignment);
      begin
         for SE of New_Alloc.all loop
            Storage_Address := SE'Address;
            exit when Storage_Address mod Alignment = 0;
         end loop;
      end;

   end Local_Pools;

   A_Pool: Local_Pools.Local_Pool;

   type Integer_Access is access Integer with Storage_Pool => A_Pool;

   X: Integer_Access := new Integer; 

begin
   null;
end;
