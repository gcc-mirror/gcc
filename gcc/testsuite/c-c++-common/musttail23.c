/* { dg-do compile } */
/* { dg-options "-W -Wall" } */

void bar (void);

void
foo (int x)
{
  __attribute__((musttail));				/* { dg-warning "empty declaration" "" { target c } } */
							/* { dg-warning "attributes at the beginning of statement are ignored" "" { target c++ } .-1 } */
  if (x == 1)
    __attribute__((musttail (1))) return bar ();	/* { dg-error "'musttail' attribute does not take any arguments" } */
  if (x == 2)
    __attribute__((musttail (1, "", 3))) return bar ();	/* { dg-error "'musttail' attribute does not take any arguments" } */
  if (x == 3)
    [[gnu::musttail (1)]] return bar ();		/* { dg-error "'musttail' attribute does not take any arguments" } */
							/* { dg-error "expected" "" { target c } .-1 } */
  if (x == 4)
    [[gnu::musttail (1, "", 3)]] return bar ();		/* { dg-error "'musttail' attribute does not take any arguments" } */
							/* { dg-error "expected" "" { target c } .-1 } */
  if (x == 3)
    [[clang::musttail (1)]] return bar ();		/* { dg-error "'musttail' attribute does not take any arguments" } */
							/* { dg-error "expected" "" { target c } .-1 } */
  if (x == 4)
    [[clang::musttail (1, "", 3)]] return bar ();	/* { dg-error "'musttail' attribute does not take any arguments" } */
							/* { dg-error "expected" "" { target c } .-1 } */
  if (x == 5)
    __attribute__((fallthrough, musttail)) return bar (); /* { dg-warning "attribute 'musttail' mixed with other attributes on 'return' statement" "" { target c } } */
							/* { dg-warning "attributes at the beginning of statement are ignored" "" { target c++ } .-1 } */

  if (x == 6)
    [[fallthrough]] [[gnu::musttail]] return bar ();	/* { dg-warning "'fallthrough' attribute ignored" "" { target c } } */
							/* { dg-warning "attributes at the beginning of statement are ignored" "" { target c++ } .-1 } */
  if (x == 7)
    [[clang::musttail, fallthrough]] return bar ();	/* { dg-warning "'fallthrough' attribute ignored" "" { target c } } */
							/* { dg-warning "attributes at the beginning of statement are ignored" "" { target c++ } .-1 } */
  if (x == 8)
    __attribute__((musttail, musttail)) return bar ();
  if (x == 9)
    [[gnu::musttail, gnu::musttail]] return bar ();
  if (x == 10)
    [[clang::musttail]] [[clang::musttail]] return bar ();
  if (x == 11)
    [[clang::musttail]] [[gnu::musttail]] return bar ();
}
