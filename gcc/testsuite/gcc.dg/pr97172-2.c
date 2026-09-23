/* PR middle-end/97172 - ICE: tree code ‘ssa_name’ is not supported in LTO
   streams
   { dg-do link }
   { dg-options "-Wall -flto -fpic -shared" }
   { dg-additional-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } }
   { dg-require-effective-target fpic }
   { dg-require-effective-target shared }
   { dg-require-effective-target lto } */

#include "pr97172.c"
