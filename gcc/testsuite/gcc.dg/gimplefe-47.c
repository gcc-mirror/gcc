/* { dg-do compile } */
/* { dg-options "-fgimple" } */

char * begfield (int tab, char * ptr, char * lim, int sword, int schar);

int __GIMPLE (ssa)
main ()
{
  char * lim;
  char * s;
  char * _1;

  __BB(2):
  _1 = begfield (58, ":ab", &__MEM <char[4]> ((void *)&":ab" + _Literal
(void *) 3), 1, 1);
  if (_1 != _Literal (char *) &__MEM <char[4]> ((void *)&":ab" + _Literal (void *) 2))
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  __builtin_abort ();

  __BB(4):
  __builtin_exit (0);
}

