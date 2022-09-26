/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

struct foo
{
  void *handle;
  void *arg;
};

void
dlinfo_doit (struct foo *args)
{
  __UINTPTR_TYPE__ **l = args->handle;

  *(__UINTPTR_TYPE__ *) args->arg = 0;
  *(__UINTPTR_TYPE__ *) args->arg = **l;
}
