// { dg-options "-fno-modules -std=c++2a" }

export // { dg-message "enabled with" }
module nope; // { dg-error "not name a type" }
// { dg-message "only available with .-fmodules." "" { target *-*-* } .-1 }
// { dg-module-cmi "!nope" }
