/* Verify that when we complain about incompatible pointer types
   involving function pointers, we show the declaration of the
   function.  */

/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* We're pretending that this is legacy code that was written before C23,
   hence it uses NULL rather than nullptr.  */
#define NULL ((void*)0)

typedef void (*void_int_fnptr_t) (int);

extern void takes_void_int_fnptr (void_int_fnptr_t fn); /* { dg-message "expected 'void_int_fnptr_t' \\{aka '\[^\n\r\]*'\\} but argument is of type '\[^\n\r\]*'" } */
extern void fn_argpass(); /* { dg-message "declared here" } */
void test_argpass ()
{
  takes_void_int_fnptr (&fn_argpass); /* { dg-error "passing argument 1 of 'takes_void_int_fnptr' from incompatible pointer type" } */
}

extern void fn_assign(); /* { dg-message "declared here" } */
void test_assign ()
{
  void (*assigned_to) (int);
  assigned_to = &fn_assign; /* { dg-error "assignment to '\[^\n\r\]*' from incompatible pointer type '\[^\n\r\]*'" } */
}

extern void fn_init(); /* { dg-message "declared here" } */
void test_init ()
{
  void (*initialized) (int) = &fn_init; /* { dg-error "initialization of '\[^\n\r\]*' from incompatible pointer type '\[^\n\r\]*'" } */
}

extern void fn_return(); /* { dg-message "declared here" } */
void_int_fnptr_t
test_return ()
{
  return &fn_return; /* { dg-error "returning '\[^\n\r\]*' from a function with incompatible return type '\[^\n\r\]*'" } */
}

/* Test of storing a sighandler_t with a function signature mismatch.
   In particular, verify that we show the locations of typedefs.  */

typedef void (*sighandler_t)(int); /* { dg-message "'sighandler_t' declared here" } */
sighandler_t signal(int signum, sighandler_t handler);

typedef void (*wrong_sighandler_t)(void); /* { dg-message "'wrong_sighandler_t' declared here" } */
extern void takes_wrong_sighandler_type (wrong_sighandler_t fn); /* { dg-message "expected 'wrong_sighandler_t' \\{aka '\[^\n\r\]*'\\} but argument is of type 'sighandler_t' \\{aka '\[^\n\r\]*'\\}" } */

void test_argpass_from_signal_result ()
{
  takes_wrong_sighandler_type (signal (42, NULL)); /* { dg-error "passing argument 1 of 'takes_wrong_sighandler_type' from incompatible pointer type" } */
}

void test_assign_from_signal_result ()
{
  wrong_sighandler_t assigned_to;
  assigned_to = signal (42, NULL); /* { dg-error "assignment to 'wrong_sighandler_t' \\{aka '\[^\n\r\]*'\\} from incompatible pointer type 'sighandler_t' \\{aka '\[^\n\r\]*'\\}" } */
}

void test_init_from_signal_result ()
{
  wrong_sighandler_t initialized = signal (42, NULL); /* { dg-error "initialization of 'wrong_sighandler_t' \\{aka '\[^\n\r\]*'\\} from incompatible pointer type 'sighandler_t' \\{aka '\[^\n\r\]*'\\}" } */
}

wrong_sighandler_t
test_return_signal_result ()
{
  return signal (42, NULL); /* { dg-error "returning 'sighandler_t' \\{aka '\[^\n\r\]*'\\} from a function with incompatible return type 'wrong_sighandler_t' \\{aka '\[^\n\r\]*'\\}" } */
}
