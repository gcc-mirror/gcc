/* PR target/69969 */
/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8" } */

int bar (int x) { return x; }
__attribute__((__target__("no-vsx"))) int foo (int x) { return x; } /* { dg-bogus "-mallow-movmisalign requires -mvsx" } */
