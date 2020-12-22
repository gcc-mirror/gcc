// { dg-additional-options -fmodules-ts }

module bob;
int k;

module :private; // { dg-error "private module fragment" }
int i;
