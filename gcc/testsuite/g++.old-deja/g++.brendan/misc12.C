// GROUPS passed miscellaneous
extern "C" void exit (int);
extern "C" int printf (const char *, ...);

/* Make sure cp-lex.c handles these properly--if this links, that means
   it emitted the strings instead of __FUNCTION__.0, etc.  */

int
main()
{
  char *a = __FUNCTION__;
  char *b = __PRETTY_FUNCTION__;

  printf ("PASS\n");
  exit (0);
}
