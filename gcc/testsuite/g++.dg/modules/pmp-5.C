// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

#define PRIVATE_SEMI private ;

module :PRIVATE_SEMI // { dg-message "sorry, unimplemented: private module fragment" }
int i;
