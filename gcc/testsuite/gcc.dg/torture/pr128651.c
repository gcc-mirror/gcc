/* { dg-do run } */

/* RPO VN used to record expressions it inserted during iteration keyed
   on optimistic value numbers of their operands, without unwinding them.
   This made later lookups pick up a wrong value.  */

void *malloc (__SIZE_TYPE__);
void *memcpy (void *, const void *, __SIZE_TYPE__);

int x = 8;
unsigned long long acc;

int
main (void)
{
  char *b = (char *) malloc (64);
  char *b2 = b + 8;
  for (int i = 0; i < 8; i++)
    ((long long *) b)[i] = 0;
  for (int k = 0; k < 4; k++)
    *(int *) (b + 8) = (int) ((unsigned) k * 3u + (unsigned) x);
  for (int k = 0; k < 4; k++)
    {
      *(long long *) (b2 + 8) = (unsigned) (k ^ x);
      unsigned long long t;
      memcpy (&t, b + 16 + k * 8, 8);
      acc += t;
      *(long long *) (b2 + 8) = 0;
      *(long long *) (b2 + 8)
	= (long long) ((unsigned long long) *(long long *) (b2 + 8) * 3u
		       + (unsigned) x);
    }
  unsigned long long h;
  memcpy (&h, b + 16, 8);
  if (h != 8)
    __builtin_abort ();
  return 0;
}
