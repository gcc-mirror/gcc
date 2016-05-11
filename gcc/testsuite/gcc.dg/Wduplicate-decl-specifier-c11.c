/* { dg-do compile } */
/* { dg-options "-std=c11 -Wduplicate-decl-specifier" } */

typedef _Atomic int AT1;
#define AT2 _Atomic int

void
foo (void)
{
  _Atomic AT1 x1;
  _Atomic AT2 x2;
  AT1 _Atomic x3;
  AT2 _Atomic x4;
  _Atomic int _Atomic x5; /* { dg-warning "duplicate ._Atomic." } */
}

void a1(_Atomic AT1 t) { }
void a2(_Atomic AT2 t) { }
void a3(AT1 _Atomic t) { }
void a4(AT2 _Atomic t) { }
void a5(_Atomic int _Atomic t) { }  /* { dg-warning "duplicate ._Atomic." } */

typedef _Atomic AT1 AAT1;
typedef _Atomic AT2 AAT2;
typedef AT1 _Atomic AT1A;
typedef AT2 _Atomic AT2A;
typedef _Atomic int _Atomic AIA;    /* { dg-warning "duplicate ._Atomic." } */
