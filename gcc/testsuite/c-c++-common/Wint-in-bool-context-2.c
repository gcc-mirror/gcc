/* { dg-options "-Wint-in-bool-context" } */
/* { dg-do compile } */

typedef unsigned u32;
typedef unsigned char u8;
#define KEYLENGTH 8

int foo (u8 plen, u32 key)
{
  if ((plen < KEYLENGTH) && (key << plen)) /* { dg-bogus "boolean context" } */
    return -1;

  if ((plen << KEYLENGTH) && (key < plen)) /* { dg-warning "boolean context" } */
    return -2;

  return 0;
}
