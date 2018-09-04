/* { dg-options "-fdiagnostics-show-caret" } */

/* Verify that we emit a "return *this;" fix-it hint for
   a missing return in an assignment operator.  */

struct s1 {
  s1& operator=(const s1&) { } // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
   s1& operator=(const s1&) { }
                              ^
                              return *this;
     { dg-end-multiline-output "" } */
};

/* Likewise for +=.  */

struct s2 {
  s2& operator+=(const s2&) {} // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
   s2& operator+=(const s2&) {}
                              ^
                              return *this;
     { dg-end-multiline-output "" } */
};

/* No warning for "void" return.  */

struct s3 {
  void operator=(const s3&) { }
};

/* We shouldn't issue the fix-it hint if the return type isn't right.  */

struct s4 {
  int operator=(int) { } // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
   int operator=(int) { }
                        ^
     { dg-end-multiline-output "" } */
};

/* Example of a multi-line fix-it hint.  */

struct s5 {
  int i;
  s5& operator=(const s5& z) {
    i = z.i;
  } // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
     i = z.i;
+    return *this;
   }
   ^
     { dg-end-multiline-output "" } */
};

/* Example of a multi-line fix-it hint with other statements.  */

extern void log (const char *);
struct s6 {
  int i;
  s6& operator=(const s6& z) {
    log ("operator=\n");
    i = z.i;
  } // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
     i = z.i;
+    return *this;
   }
   ^
     { dg-end-multiline-output "" } */
};

/* Another example of a multi-line fix-it hint with other statements.  */

struct s7 {
  int i;
  s7& operator=(const s6& z) {
    if (z.i)
      log ("operator=\n");
    else
      log ("operator=\n");
    i = z.i;
  } // { dg-warning "no return statement in function returning non-void" }
  /* { dg-begin-multiline-output "" }
     i = z.i;
+    return *this;
   }
   ^
     { dg-end-multiline-output "" } */
};
