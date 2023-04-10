--  { dg-do compile }
--  { dg-skip-if "not lock free" { hppa-*-* } }
--  { dg-options "-gnatwa -gnatVa" }

package body Prot7 is
   protected body Default_Slice is
      function Get return Instance_Pointer is
      begin
         return Default;
      end Get;

      procedure Set (
        Discard : in out Boolean;
        Slice   : in     Instance_Pointer
      ) is
      begin
         Discard := Default /= null;
         if not Discard then
            Default := Slice;
         end if;
      end Set;
   end Default_Slice;
end Prot7;
