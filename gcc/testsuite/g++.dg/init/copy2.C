// { dg-do compile }

struct S { S (); };

volatile S s[1] = { S () };
