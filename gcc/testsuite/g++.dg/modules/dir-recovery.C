// { dg-additional-options {-fmodules-ts -fpreprocessed} }
module;

; export module Hello; // { dg-error "global module fragment" }
// { dg-error "after a module interface" "" { target *-*-* } .-1 }
// { dg-error "not name a type" "" { target *-*-* } .-2 }
// { dg-message "not recognized as a module control-line" "" { target *-*-* } .-3 }

void SayHello ();
