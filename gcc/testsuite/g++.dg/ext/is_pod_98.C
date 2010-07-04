// PR c++/43333
// { dg-options "-std=c++98" }
// { dg-do run }

struct strPOD
{
  const char *const foo;
  const char *const bar;
};
extern "C" void abort (void);
int main ()
{
  if (!__is_pod (strPOD))
    abort ();
  return 0;
}
