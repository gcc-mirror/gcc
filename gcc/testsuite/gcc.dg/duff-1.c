/* Duff's device is legal C; test to make sure the compiler
   doesn't complain about it.

   Jason Thorpe <thorpej@wasabisystems.com>
   Derived from PR 3846.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);

void
duffcpy (char *dst, const char *src, unsigned long size)
{
  switch (size & 3)
    {
    for (;;)
      {
	*dst++ = *src++;
    case 3:
	*dst++ = *src++;
    case 2:
	*dst++ = *src++;
    case 1:
	*dst++ = *src++;
    case 0:
	if (size <= 3)
	  break;
	size -= 4;
      }
    }
}

const char testpat[] = "The quick brown fox jumped over the lazy dog.";

int
main()
{
  char buf[64];

  duffcpy (buf, testpat, sizeof (testpat));
  if (memcmp (buf, testpat, sizeof (testpat)) != 0)
    abort ();

  exit (0);
}
