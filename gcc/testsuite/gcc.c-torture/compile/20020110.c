/* Copyright 2002 Free Software Foundation */

/* Make sure the nested extern declaration doesn't conflict with the
   non-extern one in the enclosing scope.  */

void foo() {
  static long bar;

  {
    extern int bar;
  }
}
