/* PR tree-optimization/61869 - Spurious uninitialized warning (lim1 pass,
   pretty-printed internal var
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef struct data {
  struct data *next;
} data;

typedef struct list {
  unsigned dummy;
  struct list *next;
  data *start;
  int flags;
} list;

typedef struct iterator {
  struct data *ptr;
  unsigned dummy;
} iterator;

iterator start (list *a) {
  iterator i = {
    *(a->flags ? &a->start : 0),
    0
  };
  return i;
}

void g (iterator *i);

void f (list *b)
{
  list *a;
  iterator i;       // { dg-bogus "-Wmaybe-uninitialized" }

  for (a = b; a; a = a->next)
  for (i = start (a); i.ptr; i.ptr = i.ptr->next)
   {
     if (i.ptr)
       return;
   }

  for (a = b; a; a = a->next)
  for (i = start (a); i.ptr; i.ptr = i.ptr->next)
    g(&i);
}
