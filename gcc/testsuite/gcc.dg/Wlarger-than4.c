/* PR 90983/manual documents `-Wno-stack-usage` flag, but it is unrecognized
   { dg-do compile }
   { dg-options "-Wall -Wlarger-than=123 -Wno-larger-than" } */

char a [1234];
