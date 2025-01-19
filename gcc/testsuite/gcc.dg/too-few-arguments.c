extern void fn_a (void);
extern void fn_b (int); /* { dg-message "declared here" } */
extern void fn_c (int, int); /* { dg-message "declared here" } */
extern void fn_f (const char *, ...); /* { dg-message "declared here" } */

void test_known_fn (void)
{
  fn_a ();
  fn_b ();  /* { dg-error "too few arguments to function '\[^\n\r\]*'; expected 1, have 0" } */
  fn_c (42);/* { dg-error "too few arguments to function '\[^\n\r\]*'; expected 2, have 1" } */
  fn_f ();  /* { dg-error "too few arguments to function '\[^\n\r\]*'; expected at least 1, have 0" } */
}

struct foo
{
  void (*callback_a) (void);
  void (*callback_b) (int); /* { dg-message "declared here" } */
  void (*callback_c) (int, int); /* { dg-message "declared here" } */
};

void test_callback (struct foo *f)
{
  f->callback_a ();
  
  f->callback_b (); /* { dg-error "too few arguments to function 'f->callback_b'; expected 1, have 0" } */

  f->callback_c (42); /* { dg-error "too few arguments to function 'f->callback_c'; expected 2, have 1" } */
}
