// { dg-do assemble  }
// GROUPS passed error-messages
class A { };

int i = A::_ter;// { dg-error "" }  ._ter.*
int j = A::term;// { dg-error "" }  .term.*
