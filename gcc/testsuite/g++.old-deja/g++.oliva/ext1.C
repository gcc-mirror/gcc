// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by Michael Rosenbruch <Rosenbruch@bfw-online.de>

// Special g++ Options: 
// execution test - XFAIL *-*-*

extern "C" void abort();

int main () {
  char x[1];
  char *y = x ? /* implicit x */ : 0;
  /* For some reason, the array x is copied twice, and y points to the
     second copy (the first is never used).  If x is explicit, no copy
     is created, and the program succeeds.  */
  if (x != y) 
    abort();
}
