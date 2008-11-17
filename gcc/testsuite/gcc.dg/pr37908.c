/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-options "-Wsync-nand" } */
/* { dg-options "-Wsync-nand -march=i486" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-Wsync-nand -mcpu=v9" { target sparc*-*-* } } */


extern void abort (void);

int main (void)
{

  short xLoc;
  short xIn, xOut, xExpect, i = 1;

  xLoc = xIn = ~ (1 << i);
  xExpect = ~ (xIn & 0x7F);

  xOut = __sync_nand_and_fetch (&xLoc, 0x7F); /* { dg-message "note: '__sync_nand_and_fetch' changed semantics in GCC 4.4" "" } */

  if (xOut != xExpect)
    abort ();

  return 0;
}
