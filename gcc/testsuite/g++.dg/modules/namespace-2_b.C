// { dg-additional-options "-fmodules-ts" }

import foo;

static int also_not_exported; // ok

void X ()
{
  implicit_export::bob ();
}

static int implicit_export; // { dg-error "different kind" }

void Y ()
{
  also_not_exported = 1;
}
