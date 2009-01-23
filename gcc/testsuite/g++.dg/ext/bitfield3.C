/* { dg-do compile } */
/* { dg-options "-Wno-packed-bitfield-compat" } */

struct t
{
  char a:4;
  char b:8;
  char c:4;
} __attribute__ ((packed));

int assrt[sizeof (struct t) == 2 ? 1 : -1];
