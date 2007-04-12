extern void abort();

#define HOST_WIDE_INT long
#define HOST_BITS_PER_WIDE_INT (sizeof(long)*8)

struct tree_type
{
  unsigned int precision : 9;
};

int
sign_bit_p (struct tree_type *t, HOST_WIDE_INT val_hi, unsigned HOST_WIDE_INT val_lo)
{
  unsigned HOST_WIDE_INT mask_lo, lo;
  HOST_WIDE_INT mask_hi, hi;
  int width = t->precision;

  if (width > HOST_BITS_PER_WIDE_INT)
    {
      hi = (unsigned HOST_WIDE_INT) 1 << (width - HOST_BITS_PER_WIDE_INT - 1);
      lo = 0;

      mask_hi = ((unsigned HOST_WIDE_INT) -1
                 >> (2 * HOST_BITS_PER_WIDE_INT - width));
      mask_lo = -1;
    }
  else
    {
      hi = 0;
      lo = (unsigned HOST_WIDE_INT) 1 << (width - 1);
    
      mask_hi = 0;
      mask_lo = ((unsigned HOST_WIDE_INT) -1
                 >> (HOST_BITS_PER_WIDE_INT - width));
    }

  if ((val_hi & mask_hi) == hi
      && (val_lo & mask_lo) == lo)
    return 1;

  return 0;
}

int main()
{
  struct tree_type t;
  t.precision = 1;
  if (!sign_bit_p (&t, 0, -1))
    abort ();
  return 0;
}
