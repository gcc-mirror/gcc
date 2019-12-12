/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O1" } */

static inline void
inline_death ()
{
  __asm__ ("");			/* { dg-error "'asm' not allowed" } */
}

void
tranfunction ()
{
  __transaction_atomic
    {
      inline_death ();
    }
}
