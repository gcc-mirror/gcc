/* This checks for two things:
   - an obscure corner case in the standard rules for __LINE__
   - regression of an associated bug in cpplib where the semicolon got lost */
/* { dg-do run } */

int i = __LINE__\
;

int main (void)  /* { dg-bogus "parse error" "semicolon eaten" } */
{
  if (i != 6)
    abort ();
  else
    return 0;
}
