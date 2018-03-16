// { dg-options "-fmodules-atom" }

export module thing;
int i;
import baz; // { dg-error "expected" }

