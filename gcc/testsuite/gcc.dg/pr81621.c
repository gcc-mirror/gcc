/* PR target/81621 */
/* { dg-do compile { target freorder } } */
/* { dg-options "-Og -fno-split-wide-types -freorder-blocks-and-partition" } */

#include "graphite/scop-10.c"
