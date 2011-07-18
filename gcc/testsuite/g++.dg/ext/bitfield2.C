/* { dg-do compile } */
/* Remove pedantic.  Allow the GCC extension to use char for bitfields.  */
/* { dg-options "" } */
/* { dg-options "-Wno-packed-bitfield-compat -mno-ms-bitfields" { target { i?86-*-mingw* x86_64-*-mingw* } } } */

struct t /* { dg-message "note: offset of packed bit-field 't::b' has changed in GCC 4.4" "" { target pcc_bitfield_type_matters } } */
{
  char a:4;
  char b:8;
  char c:4;
} __attribute__ ((packed));

int assrt[sizeof (struct t) == 2 ? 1 : -1];
