// { dg-do assemble  }
// GROUPS passed operator-new
typedef __SIZE_TYPE__ size_t;
struct x { int a; void * operator new (size_t, void *); };
struct x * f(void *p) { return new (p) x; }
