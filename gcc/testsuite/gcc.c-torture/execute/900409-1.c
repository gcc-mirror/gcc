long f1(long a){return a&0xff000000L;}
long f2 (long a){return a&~0xff000000L;}
long f3(long a){return a&0x000000ffL;}
long f4(long a){return a&~0x000000ffL;}
long f5(long a){return a&0x0000ffffL;}
long f6(long a){return a&~0x0000ffffL;}

main ()
{
  long a = 0x89ABCDEF;

  if (f1(a)!=0x89000000L||
      f2(a)!=0x00ABCDEFL||
      f3(a)!=0x000000EFL||
      f4(a)!=0x89ABCD00L||
      f5(a)!=0x0000CDEFL||
      f6(a)!=0x89AB0000L)
    abort();
  exit(0);
}
