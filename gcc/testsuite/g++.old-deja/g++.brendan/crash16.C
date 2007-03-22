// { dg-do compile }
// GROUPS passed old-abort
class Graph {
public:
      unsigned         char N;
      Graph(void) {} // { dg-error "previously defined here" }
}

Graph::Graph(void)    // { dg-error "return type|redefinition|semicolon" }
{    N = 10;
}

