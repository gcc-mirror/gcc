// { dg-options "-fno-modules -std=c++17" }

export // { dg-message "ignored" }
module nope; // { dg-error "not name a type" }
// { dg-message "only available with .-fmodules." "" { target *-*-* } .-1 }
// { dg-module-cmi "!nope" }
