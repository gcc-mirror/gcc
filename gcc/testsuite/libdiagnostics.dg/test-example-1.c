/* begin quoted source */
/* Minimal usage example.  */
#include "libdiagnostics.h"

static diagnostic_manager *diag_mgr;

static void
init_diagnostics (void)
{
  diag_mgr = diagnostic_manager_new ();
  diagnostic_manager_add_text_sink (diag_mgr, stderr,
				    DIAGNOSTIC_COLORIZE_IF_TTY);
}

static void
finish_diagnostics (void)
{
  diagnostic_manager_release (diag_mgr);
}

static void
do_stuff (void)
{
  const char *username = "Dave";
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);
  diagnostic_finish (d,
		     "I'm sorry %s, I'm afraid I can't do that",
		     username);
}

int
main ()
{
  init_diagnostics ();

  do_stuff ();

  finish_diagnostics ();
};
/* end quoted source */

/* { dg-regexp "progname: error: I'm sorry Dave, I'm afraid I can't do that" } */
