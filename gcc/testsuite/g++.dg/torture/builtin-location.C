// { dg-do run }

#include <cstring>

const char *gfile;
const char *gfn;
int gline;

void bar (const char *file = __builtin_FILE (),
	  const char *function = __builtin_FUNCTION (),
	  int line = __builtin_LINE ())
{
  gfile = file;
  gfn = function;
  gline = line;
}

extern "C" void abort (void);

int main()
{
  int here;
  bar (); here = __LINE__;
  if (std::strcmp (gfn, __FUNCTION__) != 0)
    abort ();
  if (std::strcmp (gfile, __FILE__) != 0)
    abort ();
  if (gline != here)
    abort ();
  return 0;
}
