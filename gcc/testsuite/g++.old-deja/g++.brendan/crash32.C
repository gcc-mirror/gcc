// { dg-do assemble  }
// { dg-options "-fshort-enums" }
// GROUPS passed old-abort
enum Bool { FALSE, TRUE };
Bool foo () { return TRUE; }
