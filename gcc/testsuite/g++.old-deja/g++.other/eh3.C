// Special g++ Options: -O
typedef struct { } e;

char *p;

void _Jv_throw ();

int barf (int len)
{
  char a[len];

  p = a;
  _Jv_throw ();
  return 0;
}

void _Jv_throw ()
{
  e ex;
  throw ex;
}  

int main ()
{
  try  {
    barf (2);
  }
  catch (...) {
  }

  return 0;
}
