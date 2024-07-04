void foo (void);
void foo1 (int *);
void *eintr_source (void *arg)
{
  int ts = 0;
  
  if (arg)
    foo ();
  
  while (1)
    {
      if (arg)
	foo ();
      
      foo1 (&ts);
    }
  
  return 0;
}
