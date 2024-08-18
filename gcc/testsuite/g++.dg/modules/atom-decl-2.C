// { dg-additional-options "-fmodules-ts" }
export module thing;
int i;
import baz; // { dg-error "must be contiguous" }
