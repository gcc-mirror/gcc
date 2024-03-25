/* Reduced from glib, which has a g_error macro with this infinite
   loop in it:
     for (;;) ;

   Make sure we provide a readable warning for this case.  */

extern void g_log_structured_standard (const char *);

#define g_error(MSG)				\
  do {						\
    g_log_structured_standard (MSG);		\
    for (;;) ;					\
  } while (0)
/* { dg-message "5: infinite loop" "" { target *-*-* } .-2 } */

void test_g_error (void)
{
  g_error ("something went wrong"); /* { dg-message "in expansion of macro 'g_error'" } */
}
