// { dg-options "-fno-modules" }

export // { dg-message "ignored" }
module nope; // { dg-error "" }
// { dg-message "only available with -fmodules" "" { target *-*-* } .-1 }
// { dg-module-bmi "!nope" }
