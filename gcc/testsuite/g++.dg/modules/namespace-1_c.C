// { dg-additional-options "-fmodules-ts" }

static int impl;  // IF but no diagnostic required: impl@Frob not reachable from here

import Frink;

static int ompl;  // { dg-error "different kind" }
