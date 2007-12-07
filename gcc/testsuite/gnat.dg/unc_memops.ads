with System;

package Unc_Memops is
   pragma Elaborate_Body;

   type size_t is mod 2 ** Standard'Address_Size;
   subtype addr_t is System.Address;

   function  Alloc (Size : size_t) return addr_t;
   procedure Free (Ptr : addr_t);
   function  Realloc (Ptr  : addr_t; Size : size_t) return addr_t;

   procedure Expect_Symetry (Status : Boolean);
   --  Whether we expect "free"s to match "alloc" return values in
   --  reverse order, like alloc->X, alloc->Y should be followed by
   --  free Y, free X.

private

   --  Uncomment the exports below to really exercise the alternate versions.

   --  This only works when using an installed version of the tools which
   --  grabs the runtime library objects from an archive, hence doesn't force
   --  the inclusion of s-memory.o.

   --  pragma Export (C, Alloc,   "__gnat_malloc");
   --  pragma Export (C, Free,    "__gnat_free");
   --  pragma Export (C, Realloc, "__gnat_realloc");

end;
