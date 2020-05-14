// PR debug/95080
// { dg-do compile }
// { dg-options "-Og -fcse-follow-jumps -fnon-call-exceptions -fcompare-debug" }

char *a;

void baz ();

static inline bool
bar ()
{
  int j = a[0] - 1;
  switch (j)
    {
    case 0:
    case 2:
      return true;
    default:
      return false;
    }
}

static inline bool
foo ()
{
  if (bar ())
    baz ();
  return 0;
}

struct S
{
  int h;
   ~S ();
};

S::~S ()
{
  if (a[0] == 0)
    foo () != h;
}
