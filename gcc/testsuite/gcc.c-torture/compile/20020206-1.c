/* Origin: PR optimization/5429 from Philipp Thomas <pthomas@suse.de>.  */
/* This testcase caused ICE on IA-32 -O2 -march=i686 due to rtl sharing
   problem in noce_process_if_block.  Fixed by
   http://gcc.gnu.org/ml/gcc-patches/2002-01/msg02146.html.  */

typedef struct {
  unsigned char a;
} A;

unsigned int foo (A *x)
{
  unsigned char b[2] = { 0, 0 };
  unsigned char c = 0;

  c = (x->a) ? b[1] : b[0];

  return (unsigned int) c;
}
