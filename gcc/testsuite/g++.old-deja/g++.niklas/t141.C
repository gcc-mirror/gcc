// { dg-do assemble  }
// { dg-options "-Wshadow" }
// GROUPS passed niklas scoping ARM
class X { X (int); };
void X (int);// { dg-error "" } .*hides constructor.*
void f () { X (1); }
