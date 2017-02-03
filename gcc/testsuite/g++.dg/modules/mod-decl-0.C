// { dg-options "-fno-modules" }

module nope [[interface]]; // { dg-error "" }
// { dg-message "only available with -fmodules" "" { target *-*-* } 3 }
