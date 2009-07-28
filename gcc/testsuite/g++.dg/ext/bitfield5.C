/* { dg-do compile } */
/* { dg-options "-Wno-packed-bitfield-compat" } */
/* { dg-options "-Wno-packed-bitfield-compat -mno-ms-bitfields" { target i?86-*-netware } } */

struct t
{
  char a:4;
  char b:8 __attribute__ ((packed));
  char c:4;
};

int assrt[sizeof (struct t) == 2 ? 1 : -1];
