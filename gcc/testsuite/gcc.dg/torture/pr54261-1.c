/* { dg-do run } */
/* { dg-additional-options "-DSYNC_FALLBACK" { target { ! cas_int } } } */

#ifdef SYNC_FALLBACK
/* The SYNC_FALLBACK code is just so we don't have to restrict this test
   to any subset of targets.  For targets with no atomics support at
   all, the cas_int effective-target is false and the fallback provides
   a PASS.  Where the bug trigs (at the time this test-case was added),
   cas_int is also false but the fallback isn't used.  */
__attribute__((__noinline__, __noclone__))
int
# if __INT_MAX__ == 0x7fff
 __sync_fetch_and_add_2
# else
 __sync_fetch_and_add_4
# endif
 (int *at, int val)
{
  int tmp = *at;
  asm ("");
  *at = tmp + val;
  return tmp;
}
#endif

__attribute__((__noinline__, __noclone__))
void g (int *at, int val)
{
  asm ("");
  __sync_fetch_and_add (at, val);
}

int main(void)
{
  int x = 41;
  int a = 1;
  g (&x, a);

  if (x != 42)
    __builtin_abort ();
  __builtin_exit (0);
}
