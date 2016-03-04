/* PR c/69798 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

extern int foo (void);

void
fn1 (int i, int *p)
{
l:
  _Cilk_spawn (void); /* { dg-error "expected expression" } */
  _Cilk_spawn (char []); /* { dg-error "expected expression" } */
  _Cilk_spawn (int *); /* { dg-error "expected expression" } */
  _Cilk_spawn (int) 1; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn ({}); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn ++i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn i++; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn --i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn i--; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn &i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn +i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn -i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn ~i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn !i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn *p; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn &&l; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn sizeof (i); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn sizeof (short); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __alignof__ (i); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __alignof__ (short); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __extension__ i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __func__; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn p[0]; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __real__ i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn __imag__ i; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn !foo (); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn ~foo (); /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn (unsigned) foo (); /* { dg-error "only function calls can be spawned" } */
}

void
fn2 (int i, int *p)
{
l:
  _Cilk_spawn _Cilk_spawn (void); /* { dg-error "consecutive|expected expression" } */
  _Cilk_spawn _Cilk_spawn (char []); /* { dg-error "consecutive|expected expression" } */
  _Cilk_spawn _Cilk_spawn (int *); /* { dg-error "consecutive|expected expression" } */
  _Cilk_spawn _Cilk_spawn (int) 1; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn ({}); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn ++i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn i++; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn --i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn i--; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn &i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn +i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn -i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn ~i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn !i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn *p; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn &&l; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn sizeof (i); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn sizeof (short); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __alignof__ (i); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __alignof__ (short); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __extension__ i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __func__; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn p[0]; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __real__ i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn __imag__ i; /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn !foo (); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn ~foo (); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
  _Cilk_spawn _Cilk_spawn (unsigned) foo (); /* { dg-error "consecutive ._Cilk_spawn. keywords are not permitted" } */
}
