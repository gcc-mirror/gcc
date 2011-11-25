/* Test __atomic routines for existence and proper execution on 16 byte 
   values with each valid memory model.  */
/* { dg-do run } */
/* { dg-require-effective-target sync_int_128_runtime } */
/* { dg-options "-mcx16" { target { i?86-*-* x86_64-*-* } } } */

/* Test the execution of the __atomic_X builtin for a 16 byte value.  */

extern void abort(void);

__int128_t v, count, ret;

main ()
{
  v = 0;
  count = 0;

  if (__atomic_exchange_n (&v, count + 1, __ATOMIC_RELAXED) !=  count++) 
    abort ();

  if (__atomic_exchange_n (&v, count + 1, __ATOMIC_ACQUIRE) !=  count++) 
    abort ();

  if (__atomic_exchange_n (&v, count + 1, __ATOMIC_RELEASE) !=  count++) 
    abort ();

  if (__atomic_exchange_n (&v, count + 1, __ATOMIC_ACQ_REL) !=  count++) 
    abort ();

  if (__atomic_exchange_n (&v, count + 1, __ATOMIC_SEQ_CST) !=  count++) 
    abort ();

  /* Now test the generic version.  */

  count++;

  __atomic_exchange (&v, &count, &ret, __ATOMIC_RELAXED);
  if (ret != count - 1 || v != count)
    abort ();
  count++;

  __atomic_exchange (&v, &count, &ret, __ATOMIC_ACQUIRE);
  if (ret != count - 1 || v != count)
    abort ();
  count++;

  __atomic_exchange (&v, &count, &ret, __ATOMIC_RELEASE);
  if (ret != count - 1 || v != count)
    abort ();
  count++;

  __atomic_exchange (&v, &count, &ret, __ATOMIC_ACQ_REL);
  if (ret != count - 1 || v != count)
    abort ();
  count++;

  __atomic_exchange (&v, &count, &ret, __ATOMIC_SEQ_CST);
  if (ret != count - 1 || v != count)
    abort ();
  count++;

  return 0;
}
