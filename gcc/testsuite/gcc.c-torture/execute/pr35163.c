extern void abort(void);

int main()
{
  signed char a = -30;
  signed char b = -31;
  #if(__SIZEOF_INT__ >= 4)
  if (a > (unsigned short)b)
#else
  if ((long) a > (unsigned short)b)
#endif
    abort ();
  return 0;
}

