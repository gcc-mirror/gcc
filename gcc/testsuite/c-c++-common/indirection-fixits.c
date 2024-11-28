/* { dg-options "-fdiagnostics-show-caret" } */

void takes_int_ptr(int*);
void takes_char_ptr(char*);
void takes_int(int);
int returns_int(void);

int ivar;
char cvar;
int *int_ptr;
char *char_ptr;

void test_1 (void)
{
  takes_int_ptr(&ivar);
  takes_int_ptr(int_ptr);
  takes_char_ptr(&cvar);
  takes_char_ptr(char_ptr);

  ivar = 42;
  cvar = 'b';
  int_ptr = &ivar;
  char_ptr = &cvar;
}

void test_2 (void)
{
  takes_int_ptr(ivar); /* { dg-error "" "" } */
  /* { dg-message "possible fix: take the address with '&'" "" { target *-*-* } .-1 } */

  /* Expect an '&' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr(ivar);
                 ^~~~
                 |
                 int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr(ivar);
                 ^~~~
                 &
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_int_ptr(int*);
                    ^~~~
     { dg-end-multiline-output "" } */
}

void test_3 (void)
{
  takes_int_ptr(cvar); /* { dg-error "" } */

  /* Don't expect an '&' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr(cvar);
                 ^~~~
                 |
                 char
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_int_ptr(int*);
                    ^~~~
     { dg-end-multiline-output "" } */
}

void test_4 (void)
{
  takes_char_ptr(ivar); /* { dg-error "" } */

  /* Don't expect an '&' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_char_ptr(ivar);
                  ^~~~
                  |
                  int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_char_ptr(char*);
                     ^~~~~
     { dg-end-multiline-output "" } */
}

void test_5 (void)
{
  takes_char_ptr(cvar); /* { dg-error "" } */

  /* Expect an '&' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_char_ptr(cvar); 
                  ^~~~
                  |
                  char
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   takes_char_ptr(cvar); 
                  ^~~~
                  &
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_char_ptr(char*);
                     ^~~~~
     { dg-end-multiline-output "" } */
}
 
void test_6 (void)
{
  takes_int(int_ptr); /* { dg-error "" } */
  /* { dg-message "possible fix: dereference with '*'" "" { target *-*-* } .-1 } */

  /* Expect a '*' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_int(int_ptr);
             ^~~~~~~
             |
             int *
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   takes_int(int_ptr);
             ^~~~~~~
             |
             int*
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   takes_int(int_ptr);
             ^~~~~~~
             *
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_int(int);
                ^~~
     { dg-end-multiline-output "" } */
}
 
void test_7 (void)
{
  takes_int(char_ptr); /* { dg-error "" } */

  /* Don't expect a '*' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_int(char_ptr);
             ^~~~~~~~
             |
             char *
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   takes_int(char_ptr);
             ^~~~~~~~
             |
             char*
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
 void takes_int(int);
                ^~~
     { dg-end-multiline-output "" } */
}
 
void test_8 (void)
{
  ivar = int_ptr; /* { dg-error "" } */

  /* Expect a fix-it hint from the C++ FE, but not from C (due to missing
     location).  */
  /* { dg-begin-multiline-output "" }
   ivar = int_ptr;
          ^~~~~~~
          |
          int*
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   ivar = int_ptr;
          ^~~~~~~
          *
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   ivar = int_ptr;
        ^
     { dg-end-multiline-output "" { target c } } */
}

void test_9 (void)
{
  cvar = int_ptr; /* { dg-error "" } */

  /* Don't expect a '*' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   cvar = int_ptr;
          ^~~~~~~
          |
          int*
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   cvar = int_ptr;
        ^
     { dg-end-multiline-output "" { target c } } */
}

void test_10 (void)
{
  int_ptr = ivar; /* { dg-error "" } */

  /* Expect a fix-it hint from the C++ FE, but not from C (due to missing
     location).  */
  /* { dg-begin-multiline-output "" }
   int_ptr = ivar;
             ^~~~
             |
             int
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   int_ptr = ivar;
             ^~~~
             &
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   int_ptr = ivar;
           ^
     { dg-end-multiline-output "" { target c } } */
}

void test_11 (void)
{
  char_ptr = ivar; /* { dg-error "" } */

  /* Don't expect a fix-it hint, due to mismatching types.  */
  /* { dg-begin-multiline-output "" }
   char_ptr = ivar;
              ^~~~
              |
              int
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
   char_ptr = ivar;
            ^
     { dg-end-multiline-output "" { target c } } */
}

/* We shouldn't offer '&' fix-it hints for non-lvalues.  */

void test_12 (void)
{
  takes_int_ptr (returns_int ()); /* { dg-error "" } */

  /* { dg-begin-multiline-output "" }
   takes_int_ptr (returns_int ());
                  ^~~~~~~~~~~~~~
                  |
                  int
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr (returns_int ());
                  ~~~~~~~~~~~~^~
                              |
                              int
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
 void takes_int_ptr(int*);
                    ^~~~
     { dg-end-multiline-output "" } */
}

/* Ignore typedefs when offering fix-it hints.  */

typedef int typedef_int_t;
typedef_int_t typedef_int_t_var;

void test_13 (void)
{
  takes_int_ptr (typedef_int_t_var); /* { dg-error "" } */
  /* { dg-message "possible fix: take the address with '&'" "" { target *-*-* } .-1 } */

  /* Expect an '&' fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr (typedef_int_t_var);
                  ^~~~~~~~~~~~~~~~~
                  |
                  typedef_int_t {aka int}
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   takes_int_ptr (typedef_int_t_var);
                  ^~~~~~~~~~~~~~~~~
                  &
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void takes_int_ptr(int*);
                    ^~~~
     { dg-end-multiline-output "" } */
}

enum foo
{
 FOO_0,	 
 FOO_1,	 
 FOO_2
};

void test_14 (void)
{
  enum foo f;
  takes_int_ptr (f); /* { dg-error "" } */
  /* We don't expect a fix-it hint here.  */
  /* { dg-begin-multiline-output  "" }
   takes_int_ptr (f);
                  ^
                  |
                  enum foo
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output  "" }
   takes_int_ptr (f);
                  ^
                  |
                  foo
     { dg-end-multiline-output "" { target c++ } } */
  /* { dg-begin-multiline-output "" }
 void takes_int_ptr(int*);
                    ^~~~
     { dg-end-multiline-output "" } */
}
