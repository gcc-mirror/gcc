// PR c++/93809
// { dg-do compile }

typedef union{} U;
typename ::U foo () { return U();  }
