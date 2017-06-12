struct S {} f(); // { dg-error "return" "err" }
// { dg-message "note" "note" { target *-*-* } .-1 }
struct T {} *g(); // { dg-error "return" }
// { dg-message "note" "note" { target *-*-* } .-1 }
struct U {} h() {} // { dg-error "return" }
// { dg-message "note" "note" { target *-*-* } .-1 }
struct V {} *i() {} // { dg-error "return" }
// { dg-message "note" "note" { target *-*-* } .-1 }
struct W {} (*p) (); // { dg-error "return" }
// { dg-message "note" "note" { target *-*-* } .-1 }
