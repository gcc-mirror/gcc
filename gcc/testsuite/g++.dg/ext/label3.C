// Bug: we were removing the p = q assignment in dce, and then reinserting
// it *after* the try/catch in out-of-ssa.  Oops.

// testcase reduced from libjava/interpret.cc.

// { dg-do run }
// { dg-options "-O2" }

extern "C" int printf (const char *, ...);

bool b;

int main()
{
  __label__ one, two, done;
  void *labs[] = { &&one, &&two, &&done };
  const void **q = (const void **)labs;
  const void **p = q;

  try
    {
    one:
      printf ("one!\n");
      if (b)
	throw 42;
      goto **p++;

    two:
      printf ("two!\n");
      goto **p++;

    done:
      printf ("done!\n");
    }
  catch (int)
    {
      printf ("caught!\n");
    }
}
