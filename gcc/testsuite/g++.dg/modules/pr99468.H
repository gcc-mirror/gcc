// PR 99468, stupidly worded diagnostic
// { dg-additional-options -fmodule-header }

module M; // { dg-error "module-declaration not permitted" }

int i;
// { dg-prune-output "not writing" }
