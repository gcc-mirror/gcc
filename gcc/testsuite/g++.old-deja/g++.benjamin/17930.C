// { dg-do assemble  }
// 981204 bkoz
// g++/17930

char const one[] = "test";
char const two[] = one; // { dg-error "" } // ERROR -
