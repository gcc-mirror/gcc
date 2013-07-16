// Test for int128 enums.
// { dg-do compile { target int128 } }
// { dg-options "" }

enum E {
  e1 = 0xffffffffffffffff,
  e2, e3
} e = e3;

#define SA(I,X) int a##I[(X)? 1 : -1]

SA(1, sizeof(E) == sizeof(__int128));
