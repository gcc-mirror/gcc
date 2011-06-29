/* { dg-skip-if "" { "h8300*-*-*" } "*" "-msx*" }  */
/* { dg-options "-O2" } */
/* ICE for bit instruction generation using 16-bit const */

#define MSTPCRA (*(volatile unsigned char*)0xFFFFC9)
#define MSTPCRA2 (*(volatile unsigned char*)0xFFFDC8)

int
main (void)
{
  MSTPCRA = MSTPCRA2 & ~0x01;
  MSTPCRA = MSTPCRA2 ^ ~0xFE;
  MSTPCRA = MSTPCRA2 | ~0xFE;
  return 0;
}
