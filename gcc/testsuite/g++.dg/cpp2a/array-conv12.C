// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }
// { dg-options "-Wpedantic" }

int arr[1] = { 42 };
int(&r)[]{arr};
int(&r2)[] = {arr};
int(&&r3)[]{};
int(&&r4)[]{42};
int(&&r5)[] = {};
int(&&r6)[] = {42};
int(&r7)[](arr); // { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
