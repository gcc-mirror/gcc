/* Test for _Atomic in C11.  Test of valid code.  See c11-atomic-2.c
   for more exhaustive tests of assignment cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

/* The use of _Atomic as a qualifier, and of _Atomic (type-name), give
   the same type.  */
extern _Atomic int a;
extern _Atomic (int) a;
extern int *_Atomic b;
extern _Atomic (int *) b;
extern void f (int [_Atomic]);
extern void f (int *_Atomic);

/* _Atomic may be applied to arbitrary types, with or without other
   qualifiers, and assignments may be made as with non-atomic
   types.  Structure and union elements may be atomic.  */
_Atomic int ai1, ai2;
int i1;
volatile _Atomic long double ald1;
const _Atomic long double ald2;
long double ld1;
_Atomic _Complex double acd1, acd2;
_Complex double d1;
_Atomic volatile _Bool ab1;
int *p;
int *_Atomic restrict ap;
struct s { char c[1000]; };
_Atomic struct s as1;
struct s s1;
struct t { _Atomic int i; };
_Atomic struct t at1;
_Atomic struct t *atp1;
struct t t1;
union u { char c[1000]; };
_Atomic union u au1;
union u u1;
union v { _Atomic int i; };
_Atomic union v av1;
union v v1;

void
func (_Atomic volatile long al1)
{
  ai1 = ai2;
  ai1 = i1;
  i1 = ai2;
  ai1 = ald2;
  ald1 = d1;
  ld1 = acd2;
  acd1 += ab1;
  acd2 /= ai1;
  p = ap;
  ap = p;
  ab1 = p;
  as1 = s1;
  s1 = as1;
  at1 = t1;
  t1 = at1;
  /* It's unclear whether the undefined behavior (6.5.2.3#5) for
     accessing elements of atomic structures and unions is at
     translation or execution time; presume here that it's at
     execution time.  */
  t1.i = at1.i; /* { dg-warning "accessing a member .i. of an atomic structure" } */
  at1.i = t1.i; /* { dg-warning "accessing a member .i. of an atomic structure" } */
  atp1->i = t1.i; /* { dg-warning "accessing a member .i. of an atomic structure" } */
  au1 = u1;
  u1 = au1;
  av1 = v1;
  v1 = av1;
  v1.i = av1.i; /* { dg-warning "accessing a member .i. of an atomic union" } */
  av1.i = v1.i; /* { dg-warning "accessing a member .i. of an atomic union" } */
  /* _Atomic is valid on register variables, even if not particularly
     useful.  */
  register _Atomic volatile int ra1 = 1, ra2 = 2;
  ra1 = ra2;
  ra2 = ra1;
  /* And on parameters.  */
  al1 = ra1;
  ra2 = al1;
}

/* A function may return an atomic type.  */
_Atomic int
func2 (int i)
{
  return i;
}

/* Casts may specify atomic type.  */
int
func3 (int i)
{
  return func2 ((_Atomic long) i);
}

/* The _Atomic void type is valid.  */
_Atomic void *avp;

/* An array of atomic elements is valid (the elements being atomic,
   not the array).  */
_Atomic int aa[10];
int
func4 (void)
{
  return aa[2];
}

/* Increment and decrement are valid for atomic types when they are
   valid for non-atomic types.  */
void
func5 (void)
{
  ald1++;
  ald1--;
  ++ald1;
  --ald1;
  ai1++;
  ai1--;
  ++ai1;
  --ai1;
  ab1++;
  ab1--;
  ++ab1;
  --ab1;
  ap++;
  ap--;
  ++ap;
  --ap;
}

/* Compound literals may have atomic type.  */
_Atomic int *aiclp = &(_Atomic int) { 1 };

/* Test unary & and *.  */
void
func6 (void)
{
  int i = *aiclp;
  _Atomic int *p = &ai2;
}

/* Casts to atomic type are valid (although the _Atomic has little
   effect because the result is an rvalue).  */
int i2 = (_Atomic int) 1.0;

/* For pointer subtraction and comparisons, _Atomic does not count as
   a qualifier.  Likewise for conditional expressions.  */
_Atomic int *xaip1;
volatile _Atomic int *xaip2;
void *xvp1;

void
func7 (void)
{
  int r;
  r = xaip1 - xaip2;
  r = xaip1 < xaip2;
  r = xaip1 > xaip2;
  r = xaip1 <= xaip2;
  r = xaip1 >= xaip2;
  r = xaip1 == xaip2;
  r = xaip1 != xaip2;
  r = xaip1 == xvp1;
  r = xaip1 != xvp1;
  r = xvp1 == xaip1;
  r = xvp1 != xaip1;
  r = xaip1 == 0;
  r = ((void *) 0) == xaip2;
  (void) (r ? xaip1 : xaip2);
  (void) (r ? xvp1 : xaip2);
  (void) (r ? xaip2 : xvp1);
  (void) (r ? xaip1 : 0);
  (void) (r ? 0 : xaip1);
  /* The result of a conditional expression between a pointer to
     qualified or unqualified (but not atomic) void, and a pointer to
     an atomic type, is a pointer to appropriately qualified, not
     atomic, void.  As such, it is valid to use further in conditional
     expressions with other pointer types.  */
  (void) (r ? xaip1 : (r ? xaip1 : xvp1));
}

/* Pointer += and -= integer is valid.  */
void
func8 (void)
{
  b += 1;
  b -= 2ULL;
  ap += 3;
}

/* Various other cases of simple assignment are valid (some already
   tested above).  */
void
func9 (void)
{
  ap = 0;
  ap = (void *) 0;
  xvp1 = atp1;
  atp1 = xvp1;
}

/* Test compatibility of function types in cases where _Atomic matches
   (see c11-atomic-3.c for corresponding cases where it doesn't
   match).  */
void fc0a (int const);
void fc0a (int);
void fc0b (int _Atomic);
void fc0b (int _Atomic);
void fc1a (int);
void
fc1a (x)
     volatile int x;
{
}
void fc1b (_Atomic int);
void
fc1b (x)
     volatile _Atomic int x;
{
}
void
fc2a (x)
     const int x;
{
}
void fc2a (int); /* { dg-warning "follows non-prototype" } */
void
fc2b (x)
     _Atomic int x;
{
}
void fc2b (_Atomic int); /* { dg-warning "follows non-prototype" } */
void fc3a (int);
void
fc3a (x)
     volatile short x;
{
}
void fc3b (_Atomic int);
void
fc3b (x)
     _Atomic short x;
{
}
void
fc4a (x)
     const short x;
{
}
void fc4a (int); /* { dg-warning "follows non-prototype" } */
void
fc4b (x)
     _Atomic short x;
{
}
void fc4b (_Atomic int); /* { dg-warning "follows non-prototype" } */

/* Test cases involving C_MAYBE_CONST_EXPR work.  */
void
func10 (_Atomic int *p)
{
  p[0 / 0] = 1; /* { dg-warning "division by zero" } */
  p[0 / 0] += 1; /* { dg-warning "division by zero" } */
  *p = 0 / 0; /* { dg-warning "division by zero" } */
  *p += 0 / 0; /* { dg-warning "division by zero" } */
}
