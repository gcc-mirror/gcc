int
foo(void)
{
  int      a,b,g;
  int      i,len;
  int      stop;
                                                                     
  len = 10;
  stop = 0;
  for (i=0; i<len; i++)
  {
    a = bar1() ? 0 : 1;
    b = bar2() ? 0 : 1;
    g = bar3() ? 0 : 1;
                                                                     
    if (stop = ((a+b) % 2 != g)) break;
  }
 
  return stop;
}
