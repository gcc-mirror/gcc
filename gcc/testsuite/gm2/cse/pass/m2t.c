

typedef char char11[11];

struct STRING {
  char11  contents;
  int     high;
};

extern void StrIO_WriteLn (void);
static int StrLen (struct STRING a) __attribute__ ((always_inline));
static void foo (void) __attribute__ ((always_inline));


/*
 *  Function foo (foo)
 */

static void
foo (void)
{
  struct STRING t;
  char11 b;
  void * _T30;
  void * _T32;
  void * _T33;
  void * _T34;
  void * _T36;
  unsigned int _T37;
  unsigned int D_432;
  unsigned int * indirect_3;
  char11   * _T33_2;
  char11 * * indirect_1;

  __builtin_memcpy (&b, "hello", 6);
  _T30 = &t;
  _T32 = _T30;
  _T33 = &b;
  indirect_1 = (char11 * *) _T32;
  _T33_2 = (char11 *) _T33;
  *indirect_1 = _T33_2;
  _T34 = &t;
  _T36 = _T34 + 8;
  indirect_3 = (unsigned int *) _T36;
  *indirect_3 = 5;
  D_432 = StrLen (t);
  _T37 = D_432;
  if (_T37 != 5)
    goto L61;
  else
    goto L0;

 L0: ;
  StrIO_WriteLn ();

 L61: ;
  return;

}


/*
 * Function StrLen
 */

static int
StrLen (struct STRING a)
{
  void * _T18;
  void * _T20;
  char11 * _T21;
  void * _T23;
  void * _T24;
  void * _T25;
  int _T27;
  int _T28;
  char * _T29;
  unsigned int D_417;
  void * D_416;
  void * D_415;
  long int D_414;
  char11 * * indirect_0;

  _T18 = &a;
  _T20 = _T18;
  indirect_0 = (char11 * *) _T20;
  _T21 = *indirect_0;
  _T24 = _T21;
  _T23 = _T24;
  _T27 = 4;
  _T28 = _T27;
  D_414 = (long int) _T28;
  D_415 = (void *) D_414;
  D_416 = (void *)((unsigned long) D_415 + (unsigned long) _T23);
  _T29 = (char *) D_416;
  *_T29 = 97;
  D_417 = 5;
  return D_417;
}


/*
 * Function _M2_testcse49_init (_M2_testcse49_init)
 */

void
_M2_testcse49_init (void)
{
  foo ();
  return;
}


