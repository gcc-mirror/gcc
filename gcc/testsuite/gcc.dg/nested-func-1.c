/* Test for proper errors for break and continue in nested functions.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-Wno-switch-unreachable" } */

void
foo (int a)
{
  switch (a) {
    void bar1 (void) { break; } /* { dg-error "break statement" "break switch 1" } */
  }
  switch (a) {
  case 0:
    (void) 0;
    void bar2 (void) { break; } /* { dg-error "break statement" "break switch 2" } */
  }
  while (1) {
    void bar3 (void) { break; } /* { dg-error "break statement" "break while" } */
  }
  do {
    void bar4 (void) { break; } /* { dg-error "break statement" "break do" } */
  } while (1);
  for (;;) {
    void bar5 (void) { break; } /* { dg-error "break statement" "break for" } */
  }
  while (1) {
    void bar6 (void) { continue; } /* { dg-error "continue statement" "continue while" } */
  }
  do {
    void bar7 (void) { continue; } /* { dg-error "continue statement" "continue do" } */
  } while (1);
  for (;;) {
    void bar8 (void) { continue; } /* { dg-error "continue statement" "continue for" } */
  }
}
