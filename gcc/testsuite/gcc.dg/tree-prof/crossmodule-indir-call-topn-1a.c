/* It seems there is no way to avoid the other source of mulitple
   source testcase from being compiled independently.  Just avoid
   error.  */
#ifdef DOJOB
int
one (int a)
{
  return 1;
}

int
two (int a)
{
  return 0;
}
#else
int
main()
{
  return 0;
}
#endif
