// { dg-do assemble  }
// GROUPS uncaught
int a;// { dg-message "" } .*
int a;// { dg-error "" } .*
