// { dg-options "-fdiagnostics-show-caret" }

/* A collection of calls where argument 2 is of the wrong type.  */

/* decl, with argname.  */

extern int callee_1 (int one, const char **two, float three); // { dg-line callee_1 }

int test_1 (int first, const char *second, float third)
{
  return callee_1 (first, second, third); // { dg-error "27: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_1 (first, second, third);
                           ^~~~~~
                           |
                           const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_1\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } callee_1 }
  /* { dg-begin-multiline-output "" }
 extern int callee_1 (int one, const char **two, float three);
                               ~~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* decl, without argname.  */

extern int callee_2 (int, const char **, float); // { dg-line callee_2 }

int test_2 (int first, const char *second, float third)
{
  return callee_2 (first, second, third); // { dg-error "27: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_2 (first, second, third);
                           ^~~~~~
                           |
                           const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_2\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } callee_2 }
  /* { dg-begin-multiline-output "" }
 extern int callee_2 (int, const char **, float);
                           ^~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}

/* defn, with argname.  */

static int callee_3 (int one, const char **two, float three) // { dg-line callee_3 }
{
  return callee_2 (one, two, three);
}

int test_3 (int first, const char *second, float third)
{
  return callee_3 (first, second, third); // { dg-error "27: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee_3 (first, second, third);
                           ^~~~~~
                           |
                           const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_3\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } callee_3 }
  /* { dg-begin-multiline-output "" }
 static int callee_3 (int one, const char **two, float three)
                               ~~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* static member, with argname.  */

struct s4 { static int member_1 (int one, const char **two, float three); }; // { dg-line s4_member_1 }

int test_4 (int first, const char *second, float third)
{
  return s4::member_1 (first, second, third); // { dg-error "31: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return s4::member_1 (first, second, third);
                               ^~~~~~
                               |
                               const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'static int s4::member_1\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } s4_member_1 } 
  /* { dg-begin-multiline-output "" } 
 struct s4 { static int member_1 (int one, const char **two, float three); };
                                           ~~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* non-static member, with argname.  */

struct s5 { int member_1 (int one, const char **two, float three); }; // { dg-line s5_member_1 }

int test_5 (int first, const char *second, float third)
{
  s5 inst;
  return inst.member_1 (first, second, third); // { dg-error "32: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return inst.member_1 (first, second, third);
                                ^~~~~~
                                |
                                const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int s5::member_1\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } s5_member_1 } 
  /* { dg-begin-multiline-output "" }
 struct s5 { int member_1 (int one, const char **two, float three); };
                                    ~~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* non-static member, with argname, via a ptr.  */

struct s6 { int member_1 (int one, const char **two, float three); }; // { dg-line s6_member_1 }

int test_6 (int first, const char *second, float third, s6 *ptr)
{
  return ptr->member_1 (first, second, third); // { dg-error "32: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return ptr->member_1 (first, second, third);
                                ^~~~~~
                                |
                                const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int s6::member_1\\(int, const char\\*\\*, float\\)'" "" { target *-*-* } s6_member_1 } 
  /* { dg-begin-multiline-output "" } 
 struct s6 { int member_1 (int one, const char **two, float three); };
                                    ~~~~~~~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

/* Template function.  */

template <typename T>
int callee_7 (int one, T two, float three); // { dg-line callee_7_decl }

int test_7 (int first, const char *second, float third)
{
  return callee_7 <const char **> (first, second, third); // { dg-line callee_7_usage }
  // { dg-message "cannot convert 'const char\\*' to 'const char\\*\\*'" "" { target *-*-* } callee_7_usage }
  /* { dg-begin-multiline-output "" }
   return callee_7 <const char **> (first, second, third);
                                           ^~~~~~
                                           |
                                           const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int callee_7\\(int, T, float\\) .with T = const char\\*\\*.'" "" { target *-*-* } callee_7_decl }
  /* { dg-begin-multiline-output "" }
 int callee_7 (int one, T two, float three);
                        ~~^~~
     { dg-end-multiline-output "" } */
}

/* Template class, static function.  */

template <typename T>
struct s8 { static int member_1 (int one, T two, float three); }; // { dg-line s8_member_1 }

int test_8 (int first, const char *second, float third)
{
  return s8 <const char **>::member_1 (first, second, third); // { dg-error "47: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return s8 <const char **>::member_1 (first, second, third);
                                               ^~~~~~
                                               |
                                               const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'static int s8<T>::member_1\\(int, T, float\\) .with T = const char\\*\\*.'" "" { target *-*-* } s8_member_1 } 
  /* { dg-begin-multiline-output "" }
 struct s8 { static int member_1 (int one, T two, float three); };
                                           ~~^~~
     { dg-end-multiline-output "" } */
}

/* Template class, non-static function.  */

template <typename T>
struct s9 { int member_1 (int one, T two, float three); }; // { dg-line s9_member_1 }

int test_9 (int first, const char *second, float third)
{
  s9 <const char **> inst;
  return inst.member_1 (first, second, third); // { dg-error "32: cannot convert 'const char\\*' to 'const char\\*\\*'" }
  /* { dg-begin-multiline-output "" }
   return inst.member_1 (first, second, third);
                                ^~~~~~
                                |
                                const char*
     { dg-end-multiline-output "" } */
  // { dg-message "initializing argument 2 of 'int s9<T>::member_1\\(int, T, float\\) .with T = const char\\*\\*.'" "" { target *-*-* } s9_member_1 } 
  /* { dg-begin-multiline-output "" }
 struct s9 { int member_1 (int one, T two, float three); };
                                    ~~^~~
     { dg-end-multiline-output "" } */
}

/* Overloaded operator (with one candidate).  */

struct s10 {};

extern int operator- (const s10&, int); // { dg-line s10_operator }

int test_10 ()
{
  s10 v10_a, v10_b;

  return v10_a - v10_b; // { dg-error "no match for" }
  /* { dg-begin-multiline-output "" }
   return v10_a - v10_b;
          ~~~~~ ^ ~~~~~
          |       |
          s10     s10
     { dg-end-multiline-output "" } */
  // { dg-message "candidate" "" { target *-*-* } s10_operator }
  /* { dg-begin-multiline-output "" }
 extern int operator- (const s10&, int);
            ^~~~~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "no known conversion for argument 2 from" "" { target *-*-* } s10_operator }
  /* { dg-begin-multiline-output "" }
 extern int operator- (const s10&, int);
                                   ^~~
     { dg-end-multiline-output "" } */
}
