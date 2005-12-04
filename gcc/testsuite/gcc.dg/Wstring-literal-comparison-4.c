/* PR c/7776 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-string-literal-comparison" } */

int test1(char *ptr)
{
  return ptr == "foo";
}

int test2()
{
  return "foo" != (const char*)0;
}

int test3()
{
  return "foo" == (const char*)0;
}

int test4()
{
  return (const char*)0 != "foo";
}

int test5()
{
  return (const char*)0 == "foo";
}

