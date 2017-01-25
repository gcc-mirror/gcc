// { dg-options "-fno-modules" }

module nope; // { dg-error "" }
// { dg-message "only available with -fmodules" "" { target *-*-* } 3 }
