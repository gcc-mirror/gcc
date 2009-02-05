/* Bad PTA results (incorrect store handling) was causing us to delete
   *na = 0 store.  */

typedef struct E
{
  int p;
  struct E *n;
} *EP;   

typedef struct C
{
  EP x;
  short cn, cp; 
} *CP;

__attribute__((noinline)) CP
foo (CP h, EP x)
{
  EP pl = 0, *pa = &pl;
  EP nl = 0, *na = &nl;
  EP n;

  while (x)
    {
      n = x->n;   
      if ((x->p & 1) == 1) 
        {
          h->cp++;
          *pa = x;
          pa = &((*pa)->n);
        }
      else
        {
          h->cn++;
          *na = x;
          na = &((*na)->n);
        }    
      x = n;
    }
  *pa = nl;
  *na = 0;
  h->x = pl;
  return h;
}

int
main (void)
{  
  struct C c = { 0, 0, 0 };
  struct E e[2] = { { 0, &e[1] }, { 1, 0 } };
  EP p;

  foo (&c, &e[0]);
  if (c.cn != 1 || c.cp != 1)
    __builtin_abort ();
  if (c.x != &e[1])
    __builtin_abort ();
  if (e[1].n != &e[0])
    __builtin_abort ();
  if (e[0].n)
    __builtin_abort ();
  return 0;  
}


