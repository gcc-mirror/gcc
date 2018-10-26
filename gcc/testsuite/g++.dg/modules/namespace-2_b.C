// { dg-additional-options "-fmodules-ts" }

import foo;

static int also_not_exported; // ok

void X ()
{
  implicit_export::bob ();
}

// doesn't catch this collision because we don't check on insert
static int implicit_export; // { dg-error "" "" { xfail *-*-* } }

void Y ()
{
  also_not_exported = 1;
  implicit_export = 2; // { dg-error "ambiguous" }
}
