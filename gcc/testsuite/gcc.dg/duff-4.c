/* Duff's device is legal C; test to make sure the compiler
   doesn't complain about it.

   Roger Sayle <roger@eyesopen.com>
   Derived from duff-2.c.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

#if __INT_MAX__ >= 2147483647
/* At least 32-bit integers. */
typedef int type32;
#else
typedef long type32;
#endif

type32
cksum (const unsigned char *src, unsigned long size)
{
  type32 ck = 0;

  switch (size & 3)
    {
    do
      {
    case 0:
	ck ^= (type32)*src++ << 24;
	--size;
    case 3:
	ck ^= (type32)*src++ << 16;
	--size;
    case 2:
	ck ^= (type32)*src++ << 8;
	--size;
    case 1:
	ck ^= (type32)*src++;
	--size;
      }
    while (size > 0);
    }

  return ck;
}

const char testpat[] = "The quick brown fox jumped over the lazy dog.";

int
main()
{
  type32 ck;

  ck = cksum ((const unsigned char *) testpat, sizeof (testpat));
  if (ck != 925902908)
    abort ();

  exit (0);
}
