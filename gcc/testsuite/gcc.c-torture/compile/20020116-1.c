void noret (void) __attribute__ ((noreturn));
int foo (int, char **);
char *a, *b;
int d;

int
main (int argc, char **argv)
{
  register int c;

  d = 1;
  while ((c = foo (argc, argv)) != -1)
    switch (c) {
    case 's':
    case 'c':
    case 'f':
      a = b;
      break;
    case 'v':
      d = 1;
      break;
    case 'V':
      d = 0;
      break;
    }
  noret ();
  return 0;
}
