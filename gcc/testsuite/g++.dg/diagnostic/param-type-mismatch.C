// { dg-options "-fdiagnostics-show-caret" }

/* A collection of calls where argument 2 is of the wrong type.  */

/* decl, with argname.  */

extern int callee_1 (int one, const char *two, float three); // { dg-line callee_1 }

int test_1 (int first, int second, float third)
{
  return callee_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_1 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_1\\(int, const char\\*, float\\)'" "" { target *-*-* } callee_1 }
  /* { dg-begin-multiline-output "" }
 extern int callee_1 (int one, const char *two, float three);
                               ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* decl, without argname.  */

extern int callee_2 (int, const char *, float); // { dg-line callee_2 }

int test_2 (int first, int second, float third)
{
  return callee_2 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_2 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_2\\(int, const char\\*, float\\)'" "" { target *-*-* } callee_2 }
  /* { dg-begin-multiline-output "" }
 extern int callee_2 (int, const char *, float);
                           ^~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* defn, with argname.  */

static int callee_3 (int one, const char *two, float three) // { dg-line callee_3 }
{
  return callee_2 (one, two, three);
}

int test_3 (int first, int second, float third)
{
  return callee_3 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_3 (first, second, third);
                           ^~~~~~
                           |
                           int
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_3\\(int, const char\\*, float\\)'" "" { target *-*-* } callee_3 }
  /* { dg-begin-multiline-output "" }
 static int callee_3 (int one, const char *two, float three)
                               ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* static member, with argname.  */

struct s4 { static int member_1 (int one, const char *two, float three); };

int test_4 (int first, int second, float third)
{
  return s4::member_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return s4::member_1 (first, second, third);
                               ^~~~~~
                               |
                               int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 struct s4 { static int member_1 (int one, const char *two, float three); };
                                           ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* non-static member, with argname.  */

struct s5 { int member_1 (int one, const char *two, float three); };

int test_5 (int first, int second, float third)
{
  s5 inst;
  return inst.member_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return inst.member_1 (first, second, third);
                                ^~~~~~
                                |
                                int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 struct s5 { int member_1 (int one, const char *two, float three); };
                                    ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* non-static member, with argname, via a ptr.  */

struct s6 { int member_1 (int one, const char *two, float three); };

int test_6 (int first, int second, float third, s6 *ptr)
{
  return ptr->member_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return ptr->member_1 (first, second, third);
                                ^~~~~~
                                |
                                int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 struct s6 { int member_1 (int one, const char *two, float three); };
                                    ~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* Template function.  */

template <typename T>
int test_7 (int one, T two, float three);

int test_7 (int first, int second, float third)
{
  return test_7 <const char *> (first, second, third); // { dg-error "no matching function" }
  /* { dg-begin-multiline-output "" }
   return test_7 <const char *> (first, second, third);
                                                     ^
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   return test_7 <const char *> (first, second, third);
                                        ^~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 int test_7 (int one, T two, float three);
     ^~~~~~
     { dg-end-multiline-output "" } */
}

/* Template class, static function.  */

template <typename T>
struct s8 { static int member_1 (int one, T two, float three); };

int test_8 (int first, int second, float third)
{
  return s8 <const char *>::member_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return s8 <const char *>::member_1 (first, second, third);
                                              ^~~~~~
                                              |
                                              int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 struct s8 { static int member_1 (int one, T two, float three); };
                                           ~~^~~
     { dg-end-multiline-output "" } */
}

/* Template class, non-static function.  */

template <typename T>
struct s9 { int member_1 (int one, T two, float three); };

int test_9 (int first, int second, float third)
{
  s9 <const char *> inst;
  return inst.member_1 (first, second, third); // { dg-error "invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   return inst.member_1 (first, second, third);
                                ^~~~~~
                                |
                                int
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 struct s9 { int member_1 (int one, T two, float three); };
                                    ~~^~~
     { dg-end-multiline-output "" } */
}

/* Callback with name.  */

extern int callee_10 (int one, int (*two)(int, int), float three); // { dg-line callee_10 }

int test_10 (int first, int second, float third)
{
  return callee_10 (first, second, third); // { dg-error "invalid conversion from 'int' to 'int \\(\\*\\)\\(int, int\\)'" }
  /* { dg-begin-multiline-output "" }
   return callee_10 (first, second, third);
                            ^~~~~~
                            |
                            int
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_10\\(int, int \\(\\*\\)\\(int, int\\), float\\)'" "" { target *-*-* } callee_10 }
  /* { dg-begin-multiline-output "" }
 extern int callee_10 (int one, int (*two)(int, int), float three);
                                ~~~~~~^~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* Callback without name.  */

extern int callee_11 (int one, int (*)(int, int), float three); // { dg-line callee_11 }

int test_11 (int first, int second, float third)
{
  return callee_11 (first, second, third); // { dg-error "invalid conversion from 'int' to 'int \\(\\*\\)\\(int, int\\)'" }
  /* { dg-begin-multiline-output "" }
   return callee_11 (first, second, third);
                            ^~~~~~
                            |
                            int
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_11\\(int, int \\(\\*\\)\\(int, int\\), float\\)'" "" { target *-*-* } callee_11 }
  /* { dg-begin-multiline-output "" }
 extern int callee_11 (int one, int (*)(int, int), float three);
                                ^~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

// TODO: template callsite
