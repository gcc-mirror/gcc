// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }
int k;

module :private; // { dg-message "sorry, unimplemented: private module fragment" }
int i;
