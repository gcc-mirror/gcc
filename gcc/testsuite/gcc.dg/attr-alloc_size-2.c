/* PR c/36021 - __attribute__((alloc_size(n))) with function of no
   arguments causes gcc to segfault
   { dg-do compile } */

char *foo() __attribute__((alloc_size(1)));
