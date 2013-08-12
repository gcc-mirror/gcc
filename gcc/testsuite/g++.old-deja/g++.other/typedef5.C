// { dg-do assemble  }

// by Alexandre Oliva <oliva@dcc.unicamp.br>

typedef int t;
typedef t* u;
typedef u v;
typedef v* (*w)(t);
typedef int t;
typedef t* u;
typedef u v;
typedef v* (*w)(t const); // this is ok
typedef v* (*w)(t); // { dg-message "" } covers message `previously declared here'
typedef v* (*const w)(t); // { dg-error "" } invalid redeclaration
typedef v const* (*w)(t); // { dg-error "" } invalid redeclaration
typedef v* const (*w)(t); // { dg-error "" } invalid redeclaration
