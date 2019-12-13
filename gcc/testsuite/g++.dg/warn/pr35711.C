// PR 35711
// { dg-do compile }
// { dg-options "-Wcast-qual" }

int* foo (volatile int *p)
{
  return (int*)p; // { dg-warning "10:cast from type 'volatile int\\*' to type 'int\\*' casts away qualifiers" }
}
