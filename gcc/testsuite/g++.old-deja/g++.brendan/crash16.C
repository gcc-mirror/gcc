// { dg-do compile }
// { dg-options "-fshow-column" }
// GROUPS passed old-abort

class Graph { // { dg-error "1:new types|1: note: \\(perhaps" }
// { dg-error "1:return type" "" { target *-*-* } .-1 }
public:
      unsigned         char N;
      Graph(void) {} // { dg-message "7:'Graph" }
}

Graph::Graph(void) // { dg-error "1:redefinition" }
{    N = 10;
}

