// PR c++/46852
// { dg-do compile }

template
<
class
{ // { dg-error "" }
// { dg-error "-:" "" { target *-*-* } .+1 }
