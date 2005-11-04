/* { dg-do compile */
/* { dg-options "-O3 -fpic" } */

/* The web pass was creating unrecognisable pic_load_dot_plus_four insns
   on ARM.  */

__thread int a_thread_local;
void *
spin (int n)
{
  int i;
  for (i = 0; i <= n; i++)
    {
      a_thread_local += i;
    }
}
