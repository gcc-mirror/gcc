// PR 99468, stupidly worded diagnostic
// { dg-additional-options -fmodule-header }

module M; // { dg-error "module-declaration not permitted" }

inline int i;
