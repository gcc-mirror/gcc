// { dg-do assemble  }
// PRMS Id: 6837
// Bug: anonymous union confuses g++.

struct my_key {
   my_key(const my_key&);
   my_key(const char* n);
};

struct my_node {
   my_node(my_key&);
   union {
      long cnt;
      my_node* next;
   };
   my_key a;
};

extern my_node n;
my_node a(n);
