// { dg-do assemble  }
// GROUPS passed initialization
struct CharList { int i; };

const CharList& terminals = { 1 };// { dg-error "" } .*
