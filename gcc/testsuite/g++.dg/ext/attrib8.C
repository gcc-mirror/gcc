// PR 8656

extern int * (__attribute__((stdcall)) *fooPtr)( void);
int * __attribute__((stdcall)) myFn01( void) { return 0; }

void snafu( void)
{
  fooPtr = myFn01;
}
