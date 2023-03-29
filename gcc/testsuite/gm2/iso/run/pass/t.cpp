extern "C" void *malloc (int);
extern "C" void _M2_except5_init (void);
extern "C" int printf (const char *, ...);
extern "C" void Storage_ALLOCATE (void **p, unsigned int s);

void Storage_ALLOCATE (void **p, unsigned int s)
{
   (*p) = malloc (s);
}

main()
{
   try {
     _M2_except5_init ();
   }
   catch (...) {
      printf("caught in C++ main\n");
   }
}
