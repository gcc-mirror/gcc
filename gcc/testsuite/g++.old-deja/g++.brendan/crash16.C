// { dg-do assemble { xfail *-*-* } }
// GROUPS passed old-abort
class Graph {
public:
      unsigned         char N;
      Graph(void) {}; // { dg-error "" } previously defined here
}

Graph::Graph(void)
{    N = 10;// { dg-error "" }  return type.*
}

