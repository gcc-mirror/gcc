/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=separate-events" } */

extern void acquire_lock_a(void);
extern void acquire_lock_b(void);

void foo ()
{ /* { dg-message "\\(1\\) entering 'foo'" } */
  acquire_lock_a (); /* { dg-message "\\(2\\) lock a is now held by thread 1" } */
  acquire_lock_b (); /* { dg-message "\\(5\\) deadlocked due to waiting for lock b in thread 1 \\(acquired by thread 2 at \\(4\\)\\)\.\.\." } */
}

void bar ()
{ /* { dg-message "\\(3\\) entering 'bar'" } */
  acquire_lock_b (); /* { dg-message "\\(4\\) lock b is now held by thread 2" } */
  acquire_lock_a (); /* { dg-warning "deadlock due to inconsistent lock acquisition order" } */
  /* { dg-message "\\(6\\) \.\.\.whilst waiting for lock a in thread 2 \\(acquired by thread 1 at \\(2\\)\\)" "" { target *-*-* } .-1 } */
}
