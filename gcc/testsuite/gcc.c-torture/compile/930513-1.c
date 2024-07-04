/* The bit-field below would have a problem if __INT_MAX__ is too
   small.  */
#if __INT_MAX__ < 2147483647
int a;
#else
struct s {
  int f1 : 26;
  int f2 : 8;
};

int
f (struct s *x)
{
  return x->f2++ == 0;
}
#endif
