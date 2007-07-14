/* PR target/10067 */
/* Originator: <dat94ali@ludat.lth.se> */

/* { dg-do compile } */
/* { dg-options "-O2 -mtune=supersparc" } */

struct _reent;

extern unsigned long __malloc_trim_threshold;
extern unsigned long __malloc_top_pad;

int _mallopt_r(struct _reent *reent_ptr, int param_number, int value)
{
  __malloc_lock(reent_ptr);

  switch(param_number)
  {
    case -1:
      __malloc_trim_threshold = value;
      __malloc_unlock(reent_ptr);
      return 1;

    case -2:
      __malloc_top_pad = value;
      __malloc_unlock(reent_ptr);
      return 1;

    case -3:
      __malloc_unlock(reent_ptr);
      return 1;

    case -4:
      __malloc_unlock(reent_ptr);
      return value == 0;

    default:
      __malloc_unlock(reent_ptr);
      return 0;
  }
}
