/* { dg-do run } */
// { dg-additional-sources "nested-extern.cc" }
/* PR 31775 */
extern "C" void abort();
extern int *p;
int main()
{ 
  extern int i;
  i = 1;
  *p = 2;
  if (i == 2)
    abort ();
  return 0;
}

static int i;
int *p = &i;
