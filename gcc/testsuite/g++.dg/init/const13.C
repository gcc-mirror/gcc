// PR c++/87897
// { dg-do compile }

typedef struct A {} B;
A const a = B ();
