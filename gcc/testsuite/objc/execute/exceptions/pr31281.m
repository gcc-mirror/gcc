/* From PR31281.  */
extern void abort (void);
int __attribute__((noinline))
f(unsigned int i)
{
  int j, k;
  @try { } @catch(id) { return 13; }
  for (j=0; j<i; j++)
    for (k=0; k<i; k++)
      {
        @try {
          if (i)
            break;
        } @catch(id) { }
        return 9;
      }
  return 0;
}


int
main()
{
  if (f(1))
    abort ();
  return 0 ;
}
