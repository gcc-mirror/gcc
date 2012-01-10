/* dg-do run */
#define vector __attribute__((vector_size(sizeof(int)*4) ))

/* Check to make sure that we extract and insert the vector at the same
   location for vector subscripting and that vectors layout are the same
   as arrays. */

struct TV4
{
    vector int v;
};

typedef struct TV4 MYV4;
static inline int *f(MYV4 *a, int i)
{
  return &(a->v[i]);
}

static inline MYV4 myfunc2( int x, int y, int z, int w )
{
    MYV4 temp;
    *f(&temp, 0 ) = x;
    *f(&temp, 1 ) = y;
    *f(&temp, 2 ) = z;
    *f(&temp, 3 ) = w;
    return temp;
}

MYV4 val3;

__attribute__((noinline)) void modify (void) 
{
    val3 = myfunc2( 1, 2, 3, 4 );
}

int main( int argc, char* argv[] )
{
  int a[4];
  int i;
  
  modify();
  
  if (*f(&val3, 0 ) != 1)
    __builtin_abort ();
  if (*f(&val3, 1 ) != 2)
    __builtin_abort ();
  if (*f(&val3, 2 ) != 3)
    __builtin_abort ();
  if (*f(&val3, 3 ) != 4)
    __builtin_abort ();
    
  __builtin_memcpy (a, &val3, sizeof(a));
  for(i = 0; i < 4; i++)
    if (a[i] != i+1)
      __builtin_abort ();
  
  
  return 0;
}

