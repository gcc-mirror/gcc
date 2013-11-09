/* Test for _Atomic in C11.  Test of invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

/* Increment and decrement are invalid for atomic complex types and
   atomic pointers to incomplete types, just as for the corresponding
   non-atomic types.  Likewise for types on which arithmetic is
   invalid.  */
_Atomic _Complex float acf;
void *_Atomic apv;
struct s *_Atomic aps;
_Atomic struct t { char c; } as;

void
func (void)
{
  acf++; /* { dg-error "complex types" } */
  acf--; /* { dg-error "complex types" } */
  ++acf; /* { dg-error "complex types" } */
  --acf; /* { dg-error "complex types" } */
  apv++; /* { dg-error "wrong type|pointer of type" } */
  apv--; /* { dg-error "wrong type|pointer of type" } */
  ++apv; /* { dg-error "wrong type|pointer of type" } */
  --apv; /* { dg-error "wrong type|pointer of type" } */
  aps++; /* { dg-error "pointer to|invalid use of undefined type" } */
  aps--; /* { dg-error "pointer to|invalid use of undefined type" } */
  ++aps; /* { dg-error "pointer to|invalid use of undefined type" } */
  --aps; /* { dg-error "pointer to|invalid use of undefined type" } */
  as++; /* { dg-error "wrong type" } */
  as--; /* { dg-error "wrong type" } */
  ++as; /* { dg-error "wrong type" } */
  --as; /* { dg-error "wrong type" } */
}

/* Pointer subtraction and comparisons differing in _Atomic are
   invalid where such subtraction and comparisons differing in
   qualifiers are valid.  There is no special allowance for equality
   comparisons of pointers to atomic void to pointers to object
   types.  Likewise for conditional expressions.  */
int *pi;
_Atomic int *pai;
_Atomic void *pav;
int r;

void
func2 (void)
{
  r = pai - pi; /* { dg-error "invalid operands" } */
  r = pi - pai; /* { dg-error "invalid operands" } */
  r = pi < pai; /* { dg-error "distinct pointer types" } */
  r = pi > pai; /* { dg-error "distinct pointer types" } */
  r = pi <= pai; /* { dg-error "distinct pointer types" } */
  r = pi >= pai; /* { dg-error "distinct pointer types" } */
  r = pai < pi; /* { dg-error "distinct pointer types" } */
  r = pai > pi; /* { dg-error "distinct pointer types" } */
  r = pai <= pi; /* { dg-error "distinct pointer types" } */
  r = pai >= pi; /* { dg-error "distinct pointer types" } */
  r = pav == pi; /* { dg-error "distinct pointer types" } */
  r = pav != pi; /* { dg-error "distinct pointer types" } */
  r = pi == pav; /* { dg-error "distinct pointer types" } */
  r = pi != pav; /* { dg-error "distinct pointer types" } */
  (void) (r ? pai : pi); /* { dg-error "pointer type mismatch" } */
  (void) (r ? pi : pai); /* { dg-error "pointer type mismatch" } */
  (void) (r ? pai : pav); /* { dg-error "pointer type mismatch" } */
  (void) (r ? pav : pai); /* { dg-error "pointer type mismatch" } */
}

/* Likewise for pointer assignment.  */
void
func3 (void)
{
  pai = pi; /* { dg-error "incompatible pointer type" } */
  pi = pai; /* { dg-error "incompatible pointer type" } */
  pav = pai; /* { dg-error "incompatible pointer type" } */
  pai = pav; /* { dg-error "incompatible pointer type" } */
}

/* Cases that are invalid for normal assignments are just as invalid
   (and should not ICE) when the LHS is atomic.  */
void
func4 (void)
{
  as = acf; /* { dg-error "incompatible types" } */
  apv = as; /* { dg-error "incompatible types" } */
  as += 1; /* { dg-error "invalid operands" } */
  apv -= 1; /* { dg-error "pointer of type" } */
  apv *= 1; /* { dg-error "invalid operands" } */
  apv /= 1; /* { dg-error "invalid operands" } */
  apv %= 1; /* { dg-error "invalid operands" } */
  apv <<= 1; /* { dg-error "invalid operands" } */
  apv >>= 1; /* { dg-error "invalid operands" } */
  apv &= 1; /* { dg-error "invalid operands" } */
  apv ^= 1; /* { dg-error "invalid operands" } */
  apv |= 1; /* { dg-error "invalid operands" } */
}

/* We don't allow atomic bit-fields in GCC (implementation-defined
   whether they are permitted).  */
struct abf
{
  _Atomic int i : 1; /* { dg-error "atomic type" } */
  _Atomic int : 0; /* { dg-error "atomic type" } */
};

/* _Atomic (type-name) may not use a name for an array, function,
   qualified or atomic type.  */
_Atomic (int [2]) v0; /* { dg-error "array type" } */
_Atomic (void (void)) v1; /* { dg-error "function type" } */
_Atomic (_Atomic int) v2; /* { dg-error "applied to a qualified type" } */
_Atomic (const int) v3; /* { dg-error "applied to a qualified type" } */
_Atomic (volatile int) v4; /* { dg-error "applied to a qualified type" } */
_Atomic (int *restrict) v5; /* { dg-error "applied to a qualified type" } */

/* _Atomic, used as a qualifier, may not be applied to a function or
   array type.  */
typedef int arraytype[2];
typedef void functiontype (void);
_Atomic arraytype v6; /* { dg-error "array type" } */
_Atomic arraytype *v7; /* { dg-error "array type" } */
typedef _Atomic arraytype v8; /* { dg-error "array type" } */
int v9 = sizeof (_Atomic arraytype); /* { dg-error "array type" } */
void v10 (_Atomic arraytype parm); /* { dg-error "array type" } */
struct v11 { _Atomic arraytype f; }; /* { dg-error "array type" } */
_Atomic functiontype v12; /* { dg-error "function type" } */
_Atomic functiontype *v13; /* { dg-error "function type" } */
typedef _Atomic functiontype *v14; /* { dg-error "function type" } */
void v15 (_Atomic functiontype parm); /* { dg-error "function type" } */

/* Function parameters, when function types are required to be
   compatible, may not differ in the presence of _Atomic.  See
   c11-atomic-1.c for corresponding tests where _Atomic matches.  */
void fc0 (int _Atomic); /* { dg-message "previous declaration" } */
void fc0 (int); /* { dg-error "conflicting types" } */
void fc1 (int); /* { dg-message "prototype declaration" } */
void
fc1 (x)
     _Atomic int x; /* { dg-error "match prototype" } */
{
}
void
fc2 (x) /* { dg-message "previous definition" } */
     _Atomic int x;
{
}
void fc2 (int); /* { dg-error "incompatible type" } */
void fc3 (int); /* { dg-message "prototype declaration" } */
void
fc3 (x)
     _Atomic short x; /* { dg-error "match prototype" } */
{
}
void
fc4 (x) /* { dg-message "previous definition" } */
     _Atomic short x;
{
}
void fc4 (int); /* { dg-error "incompatible type" } */

/* Arrays of atomic elements cannot be initialized with string
   literals.  */
_Atomic char si0[] = ""; /* { dg-error "inappropriate type" } */
_Atomic char si1[] = u8""; /* { dg-error "inappropriate type" } */
_Atomic signed char si2[] = ""; /* { dg-error "inappropriate type" } */
_Atomic signed char si3[] = u8""; /* { dg-error "inappropriate type" } */
_Atomic unsigned char si4[] = ""; /* { dg-error "inappropriate type" } */
_Atomic unsigned char si5[] = u8""; /* { dg-error "inappropriate type" } */
_Atomic __WCHAR_TYPE__ si6[] = L""; /* { dg-error "inappropriate type" } */
_Atomic __CHAR16_TYPE__ si7[] = u""; /* { dg-error "inappropriate type" } */
_Atomic __CHAR32_TYPE__ si8[] = U""; /* { dg-error "inappropriate type" } */

/* Anything that is syntactically a qualifier applied to the (void)
   parameter list results in undefined behavior, which we
   diagnose.  */
void fv (_Atomic void); /* { dg-error "may not be qualified" } */
