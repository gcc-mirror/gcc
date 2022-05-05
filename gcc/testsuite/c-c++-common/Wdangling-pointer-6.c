/* PR middle-end/63272 - GCC should warn when using pointer to dead scoped
   variable within the same function
   Exercise -Wdangling-pointer with inlining.
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

void* sink (void*, ...);

extern int *eip;      // { dg-message "'eip' declared here" }

static inline void store (int **p, int *q)
{
  *p = q;             // { dg-warning "storing the address of local variable 'auto_x' in 'eip'" }
}

void nowarn_inlined_store_extern (void)
{
  extern int extern_x;
  store (&eip, &extern_x);
}

void nowarn_inlined_store_static (void)
{
  static int static_x;
  store (&eip, &static_x);
}

void warn_inlined_store_auto (void)
{
  int auto_x;         // { dg-message "'auto_x' declared here" }
  store (&eip, &auto_x);
}
