// Try to check that registers are preserved when the stack is unwound.
// { dg-do run }
// { dg-options -O2 }

extern "C" void exit(int);
extern "C" void abort();

// This test case triggers up to DEPTH recursive calls to function
// foo(), These calls are numbered so that 0 is the innermost, 1 the
// second innermost, and so on.  Each call caches NUM_VARS elements of
// both DOUBLE_SRC and INT_SRC and applies a trivial operation to each
// cached value.  The innermost foo() call will throw an integer call
// number.  The specified call should store its cached values in
// DOUBLE_DEST and INT_DEST, which main() will check.
const int num_vars = 16;
const int depth = 3;

float float_src[num_vars * depth];
float float_dest[num_vars];

int int_src[num_vars * depth];
int int_dest[num_vars];

void foo (int level, int throw_to)
{
  float *fsrc = &float_src[level * num_vars];
  float f00 = *fsrc++ + 1.0f;
  float f01 = *fsrc++ + 1.0f;
  float f02 = *fsrc++ + 1.0f;
  float f03 = *fsrc++ + 1.0f;
  float f04 = *fsrc++ + 1.0f;
  float f05 = *fsrc++ + 1.0f;
  float f06 = *fsrc++ + 1.0f;
  float f07 = *fsrc++ + 1.0f;
  float f08 = *fsrc++ + 1.0f;
  float f09 = *fsrc++ + 1.0f;
  float f10 = *fsrc++ + 1.0f;
  float f11 = *fsrc++ + 1.0f;
  float f12 = *fsrc++ + 1.0f;
  float f13 = *fsrc++ + 1.0f;
  float f14 = *fsrc++ + 1.0f;
  float f15 = *fsrc++ + 1.0f;

  int *isrc = &int_src[level * num_vars];
  int i00 = *isrc++ + 1;
  int i01 = *isrc++ + 1;
  int i02 = *isrc++ + 1;
  int i03 = *isrc++ + 1;
  int i04 = *isrc++ + 1;
  int i05 = *isrc++ + 1;
  int i06 = *isrc++ + 1;
  int i07 = *isrc++ + 1;
  int i08 = *isrc++ + 1;
  int i09 = *isrc++ + 1;
  int i10 = *isrc++ + 1;
  int i11 = *isrc++ + 1;
  int i12 = *isrc++ + 1;
  int i13 = *isrc++ + 1;
  int i14 = *isrc++ + 1;
  int i15 = *isrc++ + 1;

  try
    {
      if (level == 0)
	throw throw_to;
      else
	foo (level - 1, throw_to);
    }
  catch (int i)
    {
      if (i == level)
	{
	  float *fdest = float_dest;
	  *fdest++ = f00;
	  *fdest++ = f01;
	  *fdest++ = f02;
	  *fdest++ = f03;
	  *fdest++ = f04;
	  *fdest++ = f05;
	  *fdest++ = f06;
	  *fdest++ = f07;
	  *fdest++ = f08;
	  *fdest++ = f09;
	  *fdest++ = f10;
	  *fdest++ = f11;
	  *fdest++ = f12;
	  *fdest++ = f13;
	  *fdest++ = f14;
	  *fdest++ = f15;

	  int *idest = int_dest;
	  *idest++ = i00;
	  *idest++ = i01;
	  *idest++ = i02;
	  *idest++ = i03;
	  *idest++ = i04;
	  *idest++ = i05;
	  *idest++ = i06;
	  *idest++ = i07;
	  *idest++ = i08;
	  *idest++ = i09;
	  *idest++ = i10;
	  *idest++ = i11;
	  *idest++ = i12;
	  *idest++ = i13;
	  *idest++ = i14;
	  *idest++ = i15;
	}
      else
	{
	  throw;
	}
    }
}

int main ()
{
  for (int i = 0; i < depth * num_vars; i++)
    {
      int_src[i] = i * i;
      float_src[i] = i * 2.0f;
    }
  for (int level = 0; level < depth; level++)
    for (int throw_to = 0; throw_to <= level; throw_to++)
      {
	foo (level, throw_to);
	float *fsrc = &float_src[throw_to * num_vars];
	int *isrc = &int_src[throw_to * num_vars];
	for (int i = 0; i < num_vars; i++)
	  {
	    if (int_dest[i] != isrc[i] + 1)
	      abort ();
	    if (float_dest[i] != fsrc[i] + 1.0f)
	      abort ();
	  }
      }
  exit (0);
}
