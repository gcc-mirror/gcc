/* PR 17252.  When a char * pointer P takes its own address, storing
   into *P changes P itself.  */

char *a;

main ()
{
  int i;

  /* Make 'a' point to itself.  */
  a = (char *)&a;

  /* Assign NULL to 'a' byte by byte.  */
  for (i = 0; i < sizeof(char *); i++)
    a[i] = 0;

  /* If a's memory tag does not contain 'a' in its alias set, we will
     think that this predicate is superfluous and change it to
     'if (1)'.  */
  if (a == (char *)&a)
    abort ();

  return 0;
}
