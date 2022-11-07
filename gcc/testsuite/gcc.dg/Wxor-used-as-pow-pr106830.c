/* { dg-require-effective-target int128 }
   { dg-options "-Wno-pedantic" }  */

void foo0_u16_0() {
  (__int128)(18302628885633695743 << 4) ^ 0; /* { dg-warning "integer constant is so large that it is unsigned" } */
}
