/* PR middle-end/82063 - issues with arguments enabled by -Wall
   Verify that an invalid argument to -Walloc-size-larger-than is diagnosed.
   { dg-do compile }
   { dg-options "-Walloc-size-larger-than=1zb -Walloca-larger-than=2kbytes -Wvla-larger-than=3MIBZ" } */


/* { dg-error "argument to '-Walloc-size-larger-than=' should be a non-negative integer optionally followed by a size unit" "" { target *-*-* } 0 }
   { dg-error "argument to '-Walloca-larger-than=' should be a non-negative integer optionally followed by a size unit" "" { target *-*-* } 0 }
   { dg-error "argument to '-Wvla-larger-than=' should be a non-negative integer optionally followed by a size unit" "" { target *-*-* } 0 } */
