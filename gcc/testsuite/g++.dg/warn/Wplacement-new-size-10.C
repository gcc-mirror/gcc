/* PR middle-end/102243 - ICE on placement new at global scope
   { dg-do compile }
   { dg-options "-Wall" } */

void *operator new (__SIZE_TYPE__, void *);

char a[2][sizeof (int)];

int *p = new (a[1]) int;

void *operator new[] (__SIZE_TYPE__, void *p) { return p; }

int *q = new (a[1]) int[1];
