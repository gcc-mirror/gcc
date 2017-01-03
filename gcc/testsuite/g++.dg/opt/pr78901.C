// PR middle-end/78901
// { dg-do compile }
// { dg-options "-O2 -Wno-stringop-overflow" }

extern "C" int __snprintf_chk (char *, __SIZE_TYPE__, int, __SIZE_TYPE__, const char *, ...);

int
foo (char *c)
{
  try
    {
      return __snprintf_chk (c, 64, 0, 32, "%s", "abcdefghijklmnopq");
    }
  catch (...)
    {
      return -1;
    }
}
