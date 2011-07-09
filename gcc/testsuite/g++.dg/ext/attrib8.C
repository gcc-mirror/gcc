// PR 8656
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

extern int * (__attribute__((stdcall)) *fooPtr)( void);
int * __attribute__((stdcall)) myFn01( void) { return 0; }

void snafu( void)
{
  fooPtr = myFn01;
}
