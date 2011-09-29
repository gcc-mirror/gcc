#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

/* Check that constant folding in 
   these simple cases works.  */
vector (4, int)
foo (vector (4, int) x)
{
  return   (x == x) + (x != x) + (x >  x) 
	 + (x <  x) + (x >= x) + (x <= x);
}

int 
main (int argc, char *argv[])
{
  vector (4, int) t = {argc, 2, argc, 42};
  vector (4, int) r;
  int i;

  r = foo (t);

  for (i = 0; i < 4; i++)
    if (r[i] != -3)
      __builtin_abort ();

  return 0;
}
