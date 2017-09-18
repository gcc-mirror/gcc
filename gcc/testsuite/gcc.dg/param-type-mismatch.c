/* { dg-options "-fdiagnostics-show-caret" }  */

/* A collection of calls where argument 2 is of the wrong type.

   TODO: we should highlight the second parameter of the callee, rather
   than its name.  */

/* decl, with argname.  */

extern int callee_1 (int one, const char *two, float three); /* { dg-line callee_1 } */

int test_1 (int first, int second, float third)
{
  return callee_1 (first, second, third); /* { dg-warning "passing argument 2 of 'callee_1' makes pointer from integer without a cast" }  */
  /* { dg-begin-multiline-output "" }
   return callee_1 (first, second, third);
                           ^~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_1 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_1 (int one, const char *two, float three);
            ^~~~~~~~
     { dg-end-multiline-output "" } */
}

/* decl, without argname.  */

extern int callee_2 (int, const char *, float); /* { dg-line callee_2 } */

int test_2 (int first, int second, float third)
{
  return callee_2 (first, second, third); /* { dg-warning "passing argument 2 of 'callee_2' makes pointer from integer without a cast" } */
  /* { dg-begin-multiline-output "" }
   return callee_2 (first, second, third);
                           ^~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_2 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_2 (int, const char *, float);
            ^~~~~~~~
     { dg-end-multiline-output "" } */
}

/* defn, with argname.  */

static int callee_3 (int one, const char *two, float three) /* { dg-line callee_3 } */
{
  return callee_2 (one, two, three);
}

int test_3 (int first, int second, float third)
{
  return callee_3 (first, second, third); // { dg-warning "passing argument 2 of 'callee_3' makes pointer from integer without a cast" }
  /* { dg-begin-multiline-output "" }
   return callee_3 (first, second, third);
                           ^~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_3 } */
  /* { dg-begin-multiline-output "" }
 static int callee_3 (int one, const char *two, float three)
            ^~~~~~~~
     { dg-end-multiline-output "" } */
}
