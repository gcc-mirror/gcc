/* { dg-options "-fdiagnostics-show-caret -Wpointer-sign" }  */

/* A collection of calls where argument 2 is of the wrong type.
   Like param-type-mismatch.c, but expecting errors.  */

/* decl, with argname.  */

extern int callee_1 (int one, const char *two, float three); /* { dg-line callee_1 } */

int test_1 (int first, int second, float third)
{
  return callee_1 (first, second, third); /* { dg-error "passing argument 2 of 'callee_1' makes pointer from integer without a cast" }  */
  /* { dg-begin-multiline-output "" }
   return callee_1 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_1 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_1 (int one, const char *two, float three);
                               ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* decl, without argname.  */

extern int callee_2 (int, const char *, float); /* { dg-line callee_2 } */

int test_2 (int first, int second, float third)
{
  return callee_2 (first, second, third); /* { dg-error "passing argument 2 of 'callee_2' makes pointer from integer without a cast" } */
  /* { dg-begin-multiline-output "" }
   return callee_2 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_2 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_2 (int, const char *, float);
                           ^~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* defn, with argname.  */

static int callee_3 (int one, const char *two, float three) /* { dg-line callee_3 } */
{
  return callee_2 (one, two, three);
}

int test_3 (int first, int second, float third)
{
  return callee_3 (first, second, third); // { dg-error "passing argument 2 of 'callee_3' makes pointer from integer without a cast" }
  /* { dg-begin-multiline-output "" }
   return callee_3 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'const char \\*' but argument is of type 'int'" "" { target *-*-* } callee_3 } */
  /* { dg-begin-multiline-output "" }
 static int callee_3 (int one, const char *two, float three)
                               ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* Trivial decl, with argname.  */

extern int callee_4 (int one, float two, float three); /* { dg-line callee_4 } */

int test_4 (int first, const char *second, float third)
{
  return callee_4 (first, second, third); /* { dg-error "incompatible type for argument 2 of 'callee_4'" }  */
  /* { dg-begin-multiline-output "" }
   return callee_4 (first, second, third);
                           ^~~~~~
                           |
                           const char *
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'float' but argument is of type 'const char \\*'" "" { target *-*-* } callee_4 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_4 (int one, float two, float three);
                               ~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* Trivial decl, without argname.  */

extern int callee_5 (int, float, float); /* { dg-line callee_5 } */

int test_5 (int first, const char *second, float third)
{
  return callee_5 (first, second, third); /* { dg-error "incompatible type for argument 2 of 'callee_5'" }  */
  /* { dg-begin-multiline-output "" }
   return callee_5 (first, second, third);
                           ^~~~~~
                           |
                           const char *
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'float' but argument is of type 'const char \\*'" "" { target *-*-* } callee_5 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_5 (int, float, float);
                           ^~~~~
     { dg-end-multiline-output "" } */
}

/* Callback with name.  */

extern int callee_6 (int one, int (*two)(int, int), float three); /* { dg-line callee_6 } */

int test_6 (int first, int second, float third)
{
  return callee_6 (first, second, third); /* { dg-error "passing argument 2 of 'callee_6' makes pointer from integer without a cast" } */
  /* { dg-begin-multiline-output "" }
   return callee_6 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  /* { dg-message " expected 'int \\(\\*\\)\\(int,  int\\)' but argument is of type 'int'" "" { target *-*-* } callee_6 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_6 (int one, int (*two)(int, int), float three);
                               ~~~~~~^~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* Callback without name.  */

extern int callee_7 (int one, int (*)(int, int), float three); /* { dg-line callee_7 } */

int test_7 (int first, int second, float third)
{
  return callee_7 (first, second, third); /* { dg-error "passing argument 2 of 'callee_7' makes pointer from integer without a cast" } */
  /* { dg-begin-multiline-output "" }
   return callee_7 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  /* { dg-message " expected 'int \\(\\*\\)\\(int,  int\\)' but argument is of type 'int'" "" { target *-*-* } callee_7 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_7 (int one, int (*)(int, int), float three);
                               ^~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* -Wincompatible-pointer-types for a parameter.  */

extern int callee_8 (int one, float *two, float (three)); /* { dg-line callee_8 } */

int test_8 (int first, int *second, float third)
{
  return callee_8 (first, second, third); /* { dg-error "passing argument 2 of 'callee_8' from incompatible pointer type" } */
  /* { dg-begin-multiline-output "" }
   return callee_8 (first, second, third);
                           ^~~~~~
                           |
                           int *
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'float \\*' but argument is of type 'int \\*'" "" { target *-*-* } callee_8 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_8 (int one, float *two, float (three));
                               ~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* -Wpointer-sign for a parameter.  */

extern int callee_9 (int one, int *two, float (three)); /* { dg-line callee_9 } */

int test_9 (int first, unsigned int *second, float third)
{
  return callee_9 (first, second, third); /* { dg-warning "pointer targets in passing argument 2 of 'callee_9' differ in signedness" } */
  /* { dg-begin-multiline-output "" }
   return callee_9 (first, second, third);
                           ^~~~~~
                           |
                           unsigned int *
     { dg-end-multiline-output "" } */
  /* { dg-message "expected 'int \\*' but argument is of type 'unsigned int \\*'" "" { target *-*-* } callee_9 } */
  /* { dg-begin-multiline-output "" }
 extern int callee_9 (int one, int *two, float (three));
                               ~~~~~^~~
     { dg-end-multiline-output "" } */
}
