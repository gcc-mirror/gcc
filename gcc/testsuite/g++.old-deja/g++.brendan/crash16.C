// Build don't link: 
// GROUPS passed old-abort
class Graph {
public:
      unsigned         char N;
      Graph(void) {}; // ERROR - previously defined here
}

Graph::Graph(void)
{    N = 10;// ERROR -  return type.*
}

