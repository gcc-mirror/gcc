/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2 -fno-inline" } */

union U {
  __int128 i128;
  struct {
    long l1;
    long l2;
  } s;
};

union U u1,u2;

__int128
create_128 (long most_sig, long least_sig)
{
  union U u;

#if __LITTLE_ENDIAN__
  u.s.l1 = least_sig;
  u.s.l2 = most_sig;
#else
  u.s.l1 = most_sig;
  u.s.l2 = least_sig;
#endif
  return u.i128;
}

long most_sig (union U * u)
{
#if __LITTLE_ENDIAN__
  return (*u).s.l2;
#else
  return (*u).s.l1;
#endif
}

long least_sig (union U * u)
{
#if __LITTLE_ENDIAN__
  return (*u).s.l1;
#else
  return (*u).s.l2;
#endif
}

__int128
add_128 (__int128 *ptr, __int128 val)
{
	return (*ptr + val);
}

__int128
sub_128 (__int128 *ptr, __int128 val)
{
	return (*ptr - val);
}

int
main (void)
{
  /* Do a simple add/sub to make sure carry is happening between the dwords
     and that dwords are in correct endian order. */
  u1.i128 = create_128 (1, -1);
  u2.i128 = add_128 (&u1.i128, 1);
  if ((most_sig (&u2) != 2) || (least_sig (&u2) != 0))
    __builtin_abort ();
  u2.i128 = sub_128 (&u2.i128, 1);
  if ((most_sig (&u2) != 1) || (least_sig (&u2) != -1))
    __builtin_abort ();
  return 0;
}

