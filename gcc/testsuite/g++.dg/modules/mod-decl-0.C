// { dg-options "-fno-modules-ts" }

export // { dg-message "ignored" }
module nope; // { dg-error "" }
// { dg-message "only available with -fmodules-ts" "" { target *-*-* } .-1 }
// { dg-module-bmi "!nope" }
