/* { dg-options "-fdata-sections" } */

typedef unsigned char unit;
typedef unit *unitptr;
extern short global_precision;
typedef unsigned int size_t;
extern void *memcpy (void *dest, const void *src, size_t n);

short mp_compare(const unit* r1, const unit* r2)
{
  register short precision;
  precision = global_precision;
  (r1) = ((r1)+(precision)-1);
  (r2) = ((r2)+(precision)-1);
  do
    { if (*r1 < *r2)
	return(-1);
      if (*((r1)--) > *((r2)--))
	return(1);
    } while (--precision);
}

static unit modulus[((1280+(2*8))/8)];
static unit d_data[((1280+(2*8))/8)*2];

int upton_modmult (unitptr prod, unitptr multiplicand, unitptr multiplier)
{
 unitptr d = d_data;
 while (mp_compare(d,modulus) > 0)
   memcpy((void*)(prod), (const void*)(d), (global_precision));
}
