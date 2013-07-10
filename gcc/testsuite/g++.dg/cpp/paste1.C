// PR preprocessor/57757
// { dg-do compile }
// { dg-options "-std=c++11" }

#define S(x) x
extern S("C")void exit (int);
int
main ()
{
  (void) (S("foo")and 0);
  const wchar_t *p = S(L"foo")L"bar";
  const char *a = S("foo")R"(bar)";
  exit (0);
}
