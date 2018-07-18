/* Test to verify that the temporary doesn't trigger a bogus -Warray-bounds
   warning.  Distilled from libat_exchange_large_inplace in libatomic/gexch.c.
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

typedef typeof (sizeof 0) size_t;

extern void *memcpy (void*, const void*, size_t);

void libat_exchange_large_inplace (size_t n, void *mptr, void *vptr)
{
  char temp[1024];

  size_t i = 0;

  for (i = 0; n >= 1024; i += 1024, n -= 1024)
    {
      memcpy (temp, mptr + i, 1024);

      /* The memcpy call below results in the following:
	 unsigned long ivtmp.7;

	 ivtmp.7_4 = (unsigned long) mptr_9(D);
	 ...
	 <bb 4>
	 # ivtmp.7_22 = PHI <ivtmp.7_4(3), ivtmp.7_5(4)>
	 ...
	 _1 = (void *) ivtmp.7_22;
	 ...
	 memcpy (_1, _2, 1024);

	 Treating _1 as a pointer results in the bogus:
	   warning: 'memcpy' offset 0 is out of the bounds [0, 8] of object 'ivtmp.7' with type 'long unsigned int' [-Warray-bounds]
	   memcpy (mptr + i, vptr + i, 1024);
	   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      */
      memcpy (mptr + i, vptr + i, 1024);

      memcpy (vptr + i, temp, 1024);
    }
}
