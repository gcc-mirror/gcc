// PR c++/81229 GC ICE with stale pointed in identifier type.
// { dg-additional-options "--param ggc-min-heapsize=0" }

typedef unsigned L;
typedef unsigned L;

L l;
