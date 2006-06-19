/* { dg-do run } */

extern void abort(void);
struct a
{
   unsigned int bits : 1;
   signed long val : ((sizeof(long) * 8) - 1);
};
int Fnegate (struct a b)
{
  if ((-((long)b.val)) <= ((long) ((1UL << ((sizeof(long) * 8) - 2)) -1UL))
      && (-((long)b.val)) >= (-(((long) ((1UL << ((sizeof(long) * 8) - 2)) -1UL))) - 1))
     return 0 ;
  abort ();
}
int main ()
{
  struct a b = {1, 1};
  Fnegate (b);
  return 0;
}

