// { dg-additional-options -fmodules-ts }

int k;

module :private; // { dg-error "private module fragment" }
int i;
