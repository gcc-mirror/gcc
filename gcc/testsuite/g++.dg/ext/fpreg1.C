// Test permitted and invalid uses of __fpreg, for C++.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile { target ia64-*-* } }
// { dg-options "" }

__float80 f80;
double d;
// Default initialized __fpreg is OK.
__fpreg fpreg, fpreg2;
// But explicitly zero-initialized is an invalid conversion.
__fpreg fi = 0; // { dg-error "invalid conversion to '__fpreg'" }

__fpreg f0 (__fpreg);
int f1 (__float80);

// __fpreg in a structure is OK.
struct s {
  __float80 b;
  __fpreg a;
} x;

void
f (void)
{
  __fpreg *p;
  // Valid operations.
  fpreg = fpreg2;
  fpreg2 = (__fpreg) fpreg;
  fpreg = f0 (fpreg2);
  fpreg = +fpreg2;
  p = &fpreg;
  (void) fpreg;
  fpreg = x.a;
  fpreg2 = (struct s) { 0 }.a;
  fpreg = (d ? fpreg : fpreg2);
  d = sizeof (fpreg);
  (void)(fpreg, fpreg);
  // Invalid operations.
  ++fpreg; // { dg-error "invalid operation on '__fpreg'" }
  --fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg++; // { dg-error "invalid operation on '__fpreg'" }
  fpreg--; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = -fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = ~fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = !fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = *fpreg; // { dg-error "invalid type argument" }
  if (fpreg) // { dg-error "invalid conversion from '__fpreg'" }
    return;
  d = fpreg; // { dg-error "invalid conversion from '__fpreg'" }
  d = (double) fpreg; // { dg-error "invalid conversion from '__fpreg'" }
  fpreg = (__fpreg) d; // { dg-error "invalid conversion to '__fpreg'" }
  fpreg = fpreg * fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg / fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg % fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg + fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg - fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg << fpreg; // { dg-error "invalid operation on '__fpreg'" }
  fpreg = fpreg >> fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg < fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg > fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg <= fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg >= fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg == fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg != fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg & fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg ^ fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg | fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg && fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = fpreg || fpreg; // { dg-error "invalid operation on '__fpreg'" }
  d = (fpreg ? 1 : 2); // { dg-error "invalid conversion from '__fpreg'" }
  fpreg = (d ? fpreg : d); // { dg-error "invalid conversion to '__fpreg'" }
  fpreg *= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg /= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg %= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg += fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg -= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg <<= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg >>= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg &= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg ^= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
  fpreg |= fpreg; // { dg-error "invalid operation on '__fpreg'|in evaluation" }
}
