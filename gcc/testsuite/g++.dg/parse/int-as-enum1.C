// PR c++/4207
// Origin: <wanderer@rsu.ru>
// { dg-do compile }

typedef int A;
enum ::A {}; // { dg-error "" }
