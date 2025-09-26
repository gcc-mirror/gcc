// { dg-additional-options "-fmodules-ts" }

import foo;

static int also_not_exported; // { dg-error "different kind" }
static int implicit_export; // { dg-error "different kind" }
