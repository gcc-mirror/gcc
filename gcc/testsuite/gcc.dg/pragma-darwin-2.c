/* Darwin (Mac OS X) pragma exercises.  */

/* { dg-do compile { target *-*-darwin* } } */

/* The mark pragma is valid at any point in the program.  Fortunately
   the compiler only needs to ignore it.  It's also followed only
   by pp-tokens, not necessarily real C tokens.  */

void foo(void) 
{
  if (1) {
    ;
  }
  else if (1) {
    ;
  }
#pragma mark "last case" "hi"
  else if (1) {
    ;
  }
}

#pragma mark 802.11x 1_2_3
#pragma mark •••• marker ••••
