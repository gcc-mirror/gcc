// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

#define PRIVATE private

module :PRIVATE; // { dg-message "sorry, unimplemented: private module fragment" }
int i;
