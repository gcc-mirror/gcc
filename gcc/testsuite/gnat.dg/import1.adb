-- { dg-do compile }
-- { dg-options "-g" }

package body Import1 is

   procedure Create (Bounds : Arr) is
      type Bound_Array is array (Bounds'Range) of Integer;

      procedure Proc (Ptr : access Bound_Array);
      pragma Import (C, Proc);

      Temp : aliased Bound_Array;
   begin
      Proc (Temp'Access);
   end;

end Import1;
