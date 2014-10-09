/* { dg-do run } */
/* { dg-options "-O" } */

/* The following are testcases for native_interpret_int,
   native_interpret_complex and native_interpret_vector decoding
   pieces of a string constant encoded by native_encode_string.  */

extern void abort (void);

/* We should fold all reads from xconstant and eliminate it, removing
   the reference to blah which cannot be resolved at link time.  */
extern int blah;

static const struct {
    int *y;
    const char x[32] __attribute__((aligned(32)));
} xconstant = { &blah, "01234567899876543210123456789000" };

typedef int v4si __attribute__((vector_size(16)));

int main()
{
  if (sizeof (int) != 4)
    return 0;
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    {
      if (*(int *)&xconstant.x[4] != 0x34353637)
	abort ();
      if ((*(v4si *)&xconstant.x[16])[1] != 0x31323334)
	abort ();
      if (__imag (*(_Complex int *)&xconstant.x[8]) != 0x37363534)
	abort ();
    }
  else if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
    {
      if (*(int *)&xconstant.x[4] != 0x37363534)
	abort ();
      if ((*(v4si *)&xconstant.x[16])[1] != 0x34333231)
	abort ();
      if (__imag (*(_Complex int *)&xconstant.x[8]) != 0x34353637)
	abort ();
    }
  return 0;
}
