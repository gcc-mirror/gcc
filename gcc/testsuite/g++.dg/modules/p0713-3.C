// { dg-additional-options "-fmodules-ts" }
int k;
module frob; // { dg-error "global module fragment not present" }
