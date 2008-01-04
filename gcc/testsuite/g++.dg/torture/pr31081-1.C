/* { dg-do compile } */

static int get_record (void);
void f(void);
int g(void);
static int get_record (void)
{
  int     result;
  try
  {
    result = g();
    f();
  }
  catch (const int &)   { }
  return result;
}
int NAV_get_record ( )
{
  int     result;
  for (;;)
    if (get_record ())
      return 1;
}
