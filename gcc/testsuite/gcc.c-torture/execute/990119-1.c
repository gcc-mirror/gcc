/* This checks for two things:
   - an obscure corner case in the standard rules for __LINE__
   - regression of an associated bug in cpplib where the semicolon got lost */
int i = __LINE__\
;

int main (void)
{
  if (i != 4)
    abort();
  else
    return 0;
}
