// { dg-do assemble  }
// GROUPS passed niklas construct-destruct
struct S { S (); ~S (); };
void f () { while (1) S s; }

