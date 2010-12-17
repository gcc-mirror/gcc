/* { dg-do compile } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { { hppa*-*-hpux* *-*-solaris2.[56]* } && { ! hppa*64*-*-* } } { "*" } { "" } } */

/* Make sure we didn't eliminate casted types because we thought they were
   unused.  */

void *voidp;

struct foo { int i; };
int bar (void)
{
    return ((struct foo *)0x1234)->i;
}

struct boo { int i; };
int bar2 (void)
{
  return reinterpret_cast<struct boo *>(0xC0FFEE)->i;
}

struct cue { int i; };
int bar3 (void)
{
  return static_cast<struct cue *>(voidp)->i;
}

class printer { public: int i; };
const printer *dotmatrix;
int bar4 (void)
{
  return const_cast<printer *>(dotmatrix)->i;
}

class class1 { public: virtual ~class1(); } *c1;
class class2 : class1 { char j; };
int bar5 (void)
{
  if (dynamic_cast <class2 *>(c1))
    return 5;
  else
    return 6;
}
/* { dg-final { scan-assembler "foo" } } */
/* { dg-final { scan-assembler "boo" } } */
/* { dg-final { scan-assembler "cue" } } */
/* The xfail below is for PR33429.  */
/* { dg-final { scan-assembler "(string|ascii?)z?\[\t \]\"class2(\"|\\\\0)" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "(string|ascii?)z?\[\t \]\"printer(\"|\\\\0)" } } */
