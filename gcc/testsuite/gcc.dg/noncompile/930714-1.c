typedef union _yystype
{
  int i;
  int *iptr;
  int (*ifunc)(int);
  void (*vfunc)(int);
}
YYSTYPE;

extern int f1(int k);

void test()
{
  YYSTYPE a;
  int (*iptr)(int);
  int foo[5];

  a = f1;		/* { dg-error "incompatible types" } */
  a = (YYSTYPE)f1;
  a = (YYSTYPE)foo;
  a = (YYSTYPE)(int *)foo;
  iptr = f1;
  a = iptr;		/* { dg-error "incompatible types" } */
  a = (YYSTYPE)iptr;
}
