enum { TOTAL_KEYWORDS = 207 };

short int i = -1;
const char * const wordlist[TOTAL_KEYWORDS];

const char * const *
foo(void)
{
  register const char * const *wordptr = &wordlist[TOTAL_KEYWORDS + i];
  return wordptr;
}

int
main()
{
  if (foo() != &wordlist[206])
    abort ();
  exit(0);
}
