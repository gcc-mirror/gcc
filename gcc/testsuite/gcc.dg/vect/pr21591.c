/* { dg-do compile } */

struct a
{
  int length;
  int a1[256];
};

struct a *malloc1(__SIZE_TYPE__) __attribute__((malloc));
void free(void*);

void f(void)
{
   struct a *a = malloc1(sizeof(struct a));
   struct a *b = malloc1(sizeof(struct a));
   struct a *c = malloc1(sizeof(struct a));
   int i;

   for (i = 0; i < 256; i++) 
   {
      b->a1[i] = i;
      c->a1[i] = i;
   }
   for (i = 0; i < 256; i++) 
   {
      a->a1[i] = b->a1[i] + c->a1[i];
   }
   free(a);
   free(b);
   free(c);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

