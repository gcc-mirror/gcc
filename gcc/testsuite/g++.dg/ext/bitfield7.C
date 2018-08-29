/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-Wno-packed-bitfield-compat -mno-ms-bitfields" { target { i?86-*-mingw* x86_64-*-mingw* } } } */

struct t /* { dg-message "note: offset of packed bit-field 't::b' has changed in GCC 4.4" "" { target pcc_bitfield_type_matters } } */
{
  char a:4;
  char b __attribute__ ((packed)) : 8;
  char c:4;
};

int assrt[sizeof (struct t) == 2 ? 1 : -1];
