// PR middle-end/37337
// { dg-do compile }
// { dg-options "-O2" }

extern "C"
{
  typedef struct _IO_FILE FILE;
  extern int __fprintf_chk (FILE *, int, const char *, ...);
  extern inline __attribute__ ((always_inline, gnu_inline, artificial))
  int fprintf (FILE *s, const char *f, ...)
  {
    return __fprintf_chk (s, 1, f, __builtin_va_arg_pack ());
  }
}

extern int a;
struct A
{
  virtual ~A (void)
  {
  }
};

struct B : public A
{
  B ();
  FILE *b;
};

void f (int *);
B::B ()
{
  f (&a);
  for (int i = 0; i < 6; i++)
    fprintf (b, "%02x", 0xff);
  fprintf (b, "\n--\n");
}
