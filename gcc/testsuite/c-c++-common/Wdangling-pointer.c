/* PR middle-end/63272 - GCC should warn when using pointer to dead scoped
   variable within the same function
   Exercise basic cases of -Wdangling-pointer without optimization.
   { dg-do compile }
   { dg-options "-O0 -Wall -Wno-uninitialized -ftrack-macro-expansion=0" } */

typedef __INTPTR_TYPE__ intptr_t;
typedef __SIZE_TYPE__   size_t;

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void* alloca (size_t);
EXTERN_C void* malloc (size_t);
EXTERN_C void* memchr (const void*, int, size_t);
EXTERN_C char* strchr (const char*, int);

int sink (const void*, ...);
#define sink(...) sink (0, __VA_ARGS__)

/* Verify that integer assignments don't cause bogus warnings.
   Reduced from GFlibc's s_nextafter.c.  */

int nowarn_integer (float x)
{
  int i;

  {
    union
    {
      float x;
      int i;
    } u;

    u.x = x;
    i = u.i;
  }

  return i;
}

void nowarn_addr (void)
{
  int *p;
  {
    int a[] = { 1, 2, 3 };
    p = a;
  }

  // This is suspect but not a clear error.
  sink (&p);
}


char* nowarn_ptr (void)
{
  char *p;
  sink (&p);
  return p;
}


char* nowarn_cond_ptr (void)
{
  // Distilled from a false positive in Glibc dlerror.c.
  char *q;
  if (sink (&q))
    return q;

  return 0;
}


void nowarn_loop_ptr (int n, int *p)
{
  // Distilled from a false positive in Glibc td_thr_get_info.c.
  for (int i = 0; i != 2; ++i)
    {
      int x;
      sink (&x);
      *p++ = x;
    }
}


void nowarn_intptr_t (void)
{
  intptr_t ip;
  {
    int a[] = { 1, 2, 3 };
    ip = (intptr_t)a;
  }

  // Using an intptr_t is not diagnosed.
  sink (0, ip);
}


void nowarn_string_literal (void)
{
  const char *s;
  {
    s = "123";
  }

  sink (s);
}


void nowarn_extern_array (int x)
{
  {
    /* This is a silly sanity check.  */
    extern int eia[];
    int *p;
    {
      p = eia;
    }
    sink (p);
  }
}


void nowarn_static_array (int x)
{
  {
    const char *s;
    {
      static const char sca[] = "123";
      s = sca;
    }

    sink (s);
  }
  {
    const int *p;
    {
      static const int sia[] = { 1, 2, 3 };
      p = sia;
    }

    sink (p);
  }
  {
    const int *p;
    {
      static const int sia[] = { 1, 2, 3 };
      p = (const int*)memchr (sia, x, sizeof sia);
    }

    sink (p);
  }
}


void nowarn_alloca (unsigned n)
{
  {
    char *p;
    {
      p = (char*)alloca (n);
    }
    sink (p);
  }
  {
    int *p;
    {
      p = (int*)alloca (n * sizeof *p);
      sink (p);
    }
    sink (p);
  }
  {
    long *p;
    {
      p = (long*)alloca (n * sizeof *p);
      sink (p);
      p = p + 1;
    }
    sink (p);
  }
}


#pragma GCC diagnostic push
/* Verify that -Wdangling-pointer works with #pragma diagnostic.  */
#pragma GCC diagnostic ignored "-Wdangling-pointer"

void nowarn_scalar_call_ignored (void *vp)
{
  int *p;
  {
    int i;
    p = &i;
  }
  sink (p);
}

#pragma GCC diagnostic pop


void* nowarn_return_local_addr (void)
{
  int a[] = { 1, 2, 3 };
  int *p = a;

  /* This is a likely bug but it's not really one of using a dangling
     pointer but rather of returning the address of a local variable
     which is diagnosed by -Wreturn-local-addr.  */
  return p;
}

void* warn_return_local_addr (void)
{
  int *p = 0;
  {
    int a[] = { 1, 2, 3 };
    p = a;
  }

  /* Unlike the above case, here the pointer is dangling when it's
     used.  */
  return p;                   // { dg-warning "using dangling pointer 'p' to 'a'" "array" }
}


void* nowarn_return_alloca (int n)
{
  int *p = (int*)alloca (n);
  sink (p);

  /* This is a likely bug but it's not really one of using a dangling
     pointer but rather of returning the address of a local variable
     which is diagnosed by -Wreturn-local-addr.  */
  return p;
}


void warn_scalar_call (void)
{
  int *p;
  {
    int i;                    // { dg-message "'i' declared" "note" }
    p = &i;
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to 'i'" "array" }
}


void warn_array_call (void)
{
  int *p;
  {
    int a[] = { 1, 2, 3 };    // { dg-message "'a' declared" "note" }
    p = a;
  }
  sink (p);                   // { dg-warning "using dangling pointer 'p' to 'a'" "array" }
}


void* warn_array_return (void)
{
  int *p;
  {
    int a[] = { 1, 2, 3 };    // { dg-message "'a' declared" "note" }
    p = a;
  }
  return p;                   // { dg-warning "using dangling pointer 'p' to 'a'" "array" }
}


void warn_pr63272_c1 (int i)
{
  int *p = 0;

  if (i)
    {
      int k = i;              // { dg-message "'k' declared" "note" }
      p = &k;
    }

  sink (p ? *p : 0);          // { dg-warning "dangling pointer 'p' to 'k' may be used" }
}


void warn_pr63272_c4 (void)
{
  int *p = 0;

  {
    int b;                    // { dg-message "'b' declared" "note" }
    p = &b;
  }

  sink (p);                   // { dg-warning "using dangling pointer 'p' to 'b'" "scalar" }
}

void nowarn_cond_if (int i, int n)
{
  int *p;
  if (i)
    {
      int a[] = { 1, 2 };
      p = a;
      sink (p);
    }
  else
   {
     int *b = (int*)malloc (n);
     p = b;
     sink (p);
   }

  p = 0;
}


void warn_cond_if (int i, int n)
{
  int *p;
  if (i)
    {
      int a[] = { 1, 2 };     // { dg-message "'a' declared" "note" }
      sink (a);
      p = a;
    }
  else
   {
     int *b = (int*)malloc (n);
     sink (b);
     p = b;
   }

  sink (p);                   // { dg-warning "dangling pointer 'p' to 'a' may be used" }
}


void warn_cond_else (int i, int n)
{
  int *p;
  if (i)
    {
      int *a = (int*)malloc (n);
      sink (a);
      p = a;
    }
  else
   {
     int b[] = { 2, 3 };
     sink (b);
     p = b;
   }

  sink (p);                   // { dg-warning "dangling pointer 'p' to 'b' may be used" }
}


void warn_cond_if_else (int i)
{
  int *p;
  if (i)
    {
      int a[] = { 1, 2 };     // { dg-message "'a' declared" "note" }
      sink (a);
      p = a;
    }
  else
   {
     int b[] = { 3, 4 };      // { dg-message "'b' declared" "note" { xfail *-*-* } }
     sink (b);
     p = b;
   }

  /* With a PHI with more than invalid argument, only one use is diagnosed
     because after the first diagnostic the code suppresses subsequent
     ones for the same use.  This needs to be fixed.  */
  sink (p);                   // { dg-warning "dangling pointer 'p' to 'a' may be used" }
                              // { dg-warning "dangling pointer 'p' to 'b' may be used" "pr??????" { xfail *-*-* } .-1 }
}


void nowarn_gcc_i386 (int i)
{
  // Regression test reduced from gcc's i386.c.
  char a[32], *p;

  if (i != 1)
    p = a;
  else
    p = 0;

  if (i == 2)
    sink (p);
  else
    {
      if (p)
	{
	  sink (p);
	  return;
	}
      sink (p);
    }
}


void warn_memchr (char c1, char c2, char c3, char c4)
{
  char *p = 0;
  {
    char a[] = { c1, c2, c3 };// { dg-message "'a' declared" "note" }
    p = (char*)memchr (a, c4, 3);
    if (!p)
      return;
  }

  sink (p);                   // { dg-warning "using dangling pointer 'p' to 'a'" }
}


void warn_strchr (char c1, char c2, char c3, char c4)
{
  char *p = 0;
  {
    char a[] = { c1, c2, c3 }; // { dg-message "'a' declared" "note" }
    p = (char*)strchr (a, c4);
    if (!p)
      return;
  }

  sink (p);                   // { dg-warning "using dangling pointer 'p' to 'a'" }
}
