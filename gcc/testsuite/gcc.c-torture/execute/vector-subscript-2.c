#define vector __attribute__((vector_size(sizeof(int)*4) ))

/* Check to make sure that we extract and insert the vector at the same
   location for vector subscripting (with constant indexes) and
   that vectors layout are the same as arrays. */

struct TV4
{
    vector int v;
};

typedef struct TV4 MYV4;

static inline MYV4 myfunc2( int x, int y, int z, int w )
{
    MYV4 temp;
    temp.v[0] = x;
    temp.v[1] = y;
    temp.v[2] = z;
    temp.v[3] = w;
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
  
  /* Set up the vector.  */
  modify();
  
  /* Check the vector via the global variable.  */
  if (val3.v[0] != 1)
    __builtin_abort ();
  if (val3.v[1] != 2)
    __builtin_abort ();
  if (val3.v[2] != 3)
    __builtin_abort ();
  if (val3.v[3] != 4)
    __builtin_abort ();
    
  vector int a1 = val3.v;
  
   /* Check the vector via a local variable.  */
  if (a1[0] != 1)
    __builtin_abort ();
  if (a1[1] != 2)
    __builtin_abort ();
  if (a1[2] != 3)
    __builtin_abort ();
  if (a1[3] != 4)
    __builtin_abort ();
    
  __builtin_memcpy(a, &val3, sizeof(a));  
   /* Check the vector via copying it to an array.  */
  for(i = 0; i < 4; i++)
    if (a[i] != i+1)
      __builtin_abort ();
  
  
  return 0;
}

