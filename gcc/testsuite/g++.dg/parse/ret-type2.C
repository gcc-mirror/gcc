struct S {} f(); // { dg-error "" }
struct T {} *g(); // { dg-error "" }
struct U {} h() {} // { dg-error "" }
struct V {} *i() {} // { dg-error "" }
struct W {} (*p) (); // { dg-error "" }
