/* { dg-do run } */
/* { dg-options "-O2" } */
#include <stddef.h>
void *a;
int *b;
struct c {void * a;} c;
struct d {short * a;} d;

int *ip= (int *)(size_t)2;
int **ipp = &ip;

int
main()
{
  float **ptr;
  void **uptr;
  int* const* cipp = (int* const*)ipp;
  /* as an extension we consider void * universal. Writes to it should alias.  */
  asm ("":"=r"(ptr):"0"(&a));
  a=NULL;
  *ptr=(float*)(size_t)1;
  if (!a)
    __builtin_abort ();
  a=NULL;
  if (*ptr)
    __builtin_abort ();

  asm ("":"=r"(uptr):"0"(&b));
  b=NULL;
  *uptr=(void*)(size_t)1;
  if (!b)
    __builtin_abort ();
  b=NULL;
  if (*uptr)
    __builtin_abort ();

  /* Check that we disambiguate int * and char *.  */
  asm ("":"=r"(ptr):"0"(&b));
  b=NULL;
  *ptr=(float*)(size_t)1;
  if (b)
    __builtin_abort ();

  /* Again we should make void * in the structure conflict with any pointer.  */
  asm ("":"=r"(ptr):"0"(&c));
  c.a=NULL;
  *ptr=(float*)(size_t)1;
  if (!c.a)
    __builtin_abort ();
  c.a=NULL;
  if (*ptr)
    __builtin_abort ();

  asm ("":"=r"(uptr):"0"(&d));
  d.a=NULL;
  *uptr=(void*)(size_t)1;
  if (!d.a)
    __builtin_abort ();
  d.a=NULL;
  if (*uptr)
    __builtin_abort ();

  if ((void *)*cipp != (void*)(size_t)2)
    __builtin_abort ();
  *ipp = NULL;
  if (*cipp)
    __builtin_abort ();

  return 0;
}
