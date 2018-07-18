// PR c++/79085
// { dg-do compile }
// { dg-options "-Os" }
// { dg-additional-options "-mstrict-align" { target { aarch64*-*-* powerpc*-*-linux* powerpc*-*-elf* } } }

void *operator new (__SIZE_TYPE__, void *p) { return p; }

struct S
{
  S ();
  S (const S &);
  ~S (void);
  int i;
};

S foo ();

static char buf [sizeof (S) + 1];

S *
bar ()
{
  return new (buf + 1) S (foo ());
}
