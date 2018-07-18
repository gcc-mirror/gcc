// { dg-do run }

#include <string.h>

const char *ct, *dt, *cv;

struct KLASS
{
  KLASS () ;
  ~KLASS ();
  operator int ();
};

KLASS::KLASS()
{
  ct = __builtin_FUNCTION ();
}

KLASS::~KLASS ()
{
  dt = __builtin_FUNCTION ();
}

KLASS::operator int ()
{
  cv = __builtin_FUNCTION ();
  return 0;
}

int main ()
{
  int q = int (KLASS ());

  if (strcmp (ct, "KLASS"))
    return 1;
  if (strcmp (dt, "~KLASS"))
    return 2;
  if (strcmp (cv, "operator int"))
    return 3;

  return 0;
}
