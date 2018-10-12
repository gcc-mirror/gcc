// { dg-options "-fmodules-atom" }

int i;
import baz; // { dg-error "must be within module preamble" }
