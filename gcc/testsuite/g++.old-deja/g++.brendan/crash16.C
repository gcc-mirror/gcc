// { dg-do compile }
// { dg-options "-fshow-column" }
// GROUPS passed old-abort

class Graph { // { dg-error "1: error: new types|1: note: \\(perhaps" }
public:
      unsigned         char N;
      Graph(void) {} // { dg-error "17: error: 'Graph" }
}

Graph::Graph(void) // { dg-error "18: error: return type|18: error: redefinition" }
{    N = 10;
}

