extern void doit(int);
void 
quick_doit(int x)
{
#ifdef __OPTIMIZE__
  if (__builtin_constant_p (x)
      && x != 0)
    asm volatile ("%0" : : "i#*X"(x));
  else
#endif
    doit(x);
}
