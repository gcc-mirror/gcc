// Build don't link:
// Special g++ Options: -fno-rtti

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Apr 1999 <nathan@acm.org>
// derrived from bug report from Alexander Zvyagin <zvyagin@mx.ihep.su>

// check we don't die with disabled rtti


int main(void) {
  int i;
  typeid(i); // ERROR - rtti disabled
}
