// Build don't link:

// by Alexandre Oliva <oliva@dcc.unicamp.br>

typedef int t;
typedef t* u;
typedef u v;
typedef v* (*w)(t);
typedef int t;
typedef t* u;
typedef u v;
typedef v* (*w)(t const); // this is ok
typedef v* (*w)(t); // ERROR - covers message `previously declared here'
typedef v* (*const w)(t); // ERROR - invalid redeclaration
typedef v const* (*w)(t); // ERROR - invalid redeclaration
typedef v* const (*w)(t); // ERROR - invalid redeclaration
