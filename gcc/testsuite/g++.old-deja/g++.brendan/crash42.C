// { dg-do assemble  }
// GROUPS passed old-abort
int fn();// { dg-error "" }  ambiguates.*
int x;
int& fn() {// { dg-error "" }  new decl.*
return x;}
