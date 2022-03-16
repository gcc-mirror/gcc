/* PR rtl-optimization/104777 */
/* { dg-do compile } */
/* { dg-require-effective-target tls } */
 
int savestate_r;
int savestate_ssb;
extern void abort();
__thread int  loop;
void f (void)
{
  int savestate_r0_5;
  int savestate_r1_6;

  __asm__("" : "=m" (savestate_ssb), "=r" (savestate_r));
  savestate_r0_5 = savestate_r;
  if (savestate_r0_5 == 0)
  {
    __asm__ __volatile__("" :  : "m" (loop));
    abort ();
  }

  __asm__("" : "=m" (savestate_ssb), "=r" (savestate_r));
  savestate_r1_6 = savestate_r;
  if (savestate_r1_6 != 0)
    return;

  __asm__ __volatile__("" :  : "m" (loop));
  abort ();

}
