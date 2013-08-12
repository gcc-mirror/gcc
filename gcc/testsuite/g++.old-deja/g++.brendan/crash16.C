// { dg-do compile }
// { dg-options "-fshow-column" }
// GROUPS passed old-abort

class Graph { // { dg-error "1:new types|1: note: \\(perhaps" }
public:
      unsigned         char N;
      Graph(void) {} // { dg-message "7:'Graph" }
}

Graph::Graph(void) // { dg-error "18:return type|1: error: redefinition" }
{    N = 10;
}

