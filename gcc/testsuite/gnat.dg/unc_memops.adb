
package body Unc_Memops is

   use type System.Address;

   type Addr_Array_T is array (1 .. 20) of Addr_T;

   type Addr_Stack_T is record
      Store : Addr_Array_T;
      Size  : Integer := 0;
   end record;

   procedure Push (Addr : Addr_T; As : access addr_stack_t) is
   begin
      As.Size := As.Size + 1;
      As.Store (As.Size) := Addr;
   end;

   function Pop (As : access Addr_Stack_T) return Addr_T is
      Addr : Addr_T := As.Store (As.Size);
   begin
      As.Size := As.Size - 1;
      return Addr;
   end;

   --

   Addr_Stack : aliased Addr_Stack_T;
   Symetry_Expected : Boolean := False;

   procedure Expect_Symetry (Status : Boolean) is
   begin
      Symetry_Expected := Status;
   end;

   function  Alloc (Size : size_t) return Addr_T is
      function malloc (Size : Size_T) return Addr_T;
      pragma Import (C, Malloc, "malloc");

      Ptr : Addr_T := malloc (Size);
   begin
      if Symetry_Expected then
         Push (Ptr, Addr_Stack'Access);
      end if;
      return Ptr;
   end;

   procedure Free (Ptr : addr_t) is
   begin
      if Symetry_Expected
        and then Ptr /= Pop (Addr_Stack'Access)
      then
         raise Program_Error;
      end if;
   end;

   function  Realloc (Ptr  : addr_t; Size : size_t) return Addr_T is
   begin
      raise Program_Error;
      return System.Null_Address;
   end;

end;
