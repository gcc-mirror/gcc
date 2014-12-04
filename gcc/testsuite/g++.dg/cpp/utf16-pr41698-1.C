// PR 41698: off-by-one error in UTF-16 encoding.

// { dg-do run { target c++11 } }

extern "C" void abort (void);
extern "C" void exit (int);

int
main ()
{
  char16_t s[] = u"\uffff";
  if (sizeof s != 2 * sizeof (char16_t) || s[0] != 0xffff || s[1] != 0)
    abort ();
  exit (0);
}
