/* This used to ICE on s390x due to a reload bug.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

int gl2;
typedef __SIZE_TYPE__ size_t;

extern void *memcpy (void *dest, const void *src, size_t n);

void
f1 ()
{
  int i2;
  unsigned char bf[64 * 1024 + 4];

  for (i2 = 0; i2 < 3; i2++)
    {
      unsigned char *p2 = bf;
      unsigned char *p3 = ((void *) 0);
      unsigned short ctf2;

      p2 += sizeof (short);

      for (ctf2 = 0; ctf2 < 3; ctf2++)
	{
	  if (ctf2 == 1)
	    {
	      unsigned short of = p2 - bf - 6;
	      unsigned short *ofp = (unsigned short *) &of;
	      memcpy (p3, ofp, sizeof (short));
	    }

	  if (gl2 == 1)
	    p2 += 3;
	}
    }
}
