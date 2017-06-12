/* PR c/59717 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11 -Wno-implicit-function-declaration" } */

void
math (double d, int *ex, double *dp)
{
  acos (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  acosh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  asin (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  asinh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  atan (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  atanh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  atan2 (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  cbrt (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  ceil (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  copysign (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  cos (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  cosh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  erf (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  erfc (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  exp (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  exp2 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  expm1 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fabs (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fdim (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  floor (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fma (d, d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fmax (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fmin (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  fmod (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  frexp (d, ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  hypot (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  /* We don't generate the warning for ilogb.  */
  ilogb (d);
  ldexp (d, *ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  lgamma (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  llrint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  llround (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  log (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  log10 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  log1p (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  log2 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  logb (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  lrint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  lround (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  modf (d, dp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  nan (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  nearbyint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  nextafter (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  nexttoward (d, 20.0L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  pow (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  remainder (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  remquo (d, d, ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  rint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  round (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  scalbln (d, 100L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  scalbn (d, 100); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  sin (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  sinh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  sincos (d, dp, dp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  sqrt (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  tan (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  tanh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  tgamma (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
  trunc (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } .-1 } */
}

void
cmplx (double _Complex z)
{
  cabs (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cacos (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cacosh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  carg (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  casin (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  casinh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  catan (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  catanh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  ccos (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  ccosh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cexp (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cimag (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  clog (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  conj (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cpow (z, z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  cproj (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  creal (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  csin (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  csinh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  csqrt (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  ctan (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
  ctanh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } .-1 } */
}

void
string (void *p, void *q, __SIZE_TYPE__ sz)
{
  memchr (p, 2, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  /* We don't generate the warning for memcmp.  */
  memcmp (p, q, sz);
  memcpy (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  memmove (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  memset (p, 0, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strcat (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strchr (p, 'a'); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  /* We don't generate the warning for strcmp.  */
  strcmp (p, q);
  strcpy (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strcspn (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strlen (p); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strncat (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  /* We don't generate the warning for strncmp.  */
  strncmp (p, q, sz);
  strncpy (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strpbrk (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strrchr (p, 'q'); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strspn (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
  strstr (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } .-1 } */
}

/* Fake FILE.  */
typedef struct { int i; } FILE;

void
stdio (FILE *fp, void *p, __SIZE_TYPE__ sz)
{
  fprintf (fp, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  fscanf (fp, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  fwrite (p, sz, sz, fp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  printf (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  scanf (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  snprintf ("", sz, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  sprintf ("", ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
  sscanf ("", ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } .-1 } */
}

void
stdlib (void *p, void *q, __SIZE_TYPE__ sz)
{
  abort (); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  calloc (sz, 1); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  exit (1); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  free (p); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  labs (1L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  llabs (1LL); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  malloc (sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  realloc (p, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
  aligned_alloc (sz, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } .-1 } */
}

void
inttypes (__INTMAX_TYPE__ j)
{
  imaxabs (j); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..inttypes.h.." "" { target *-*-* } .-1 } */
}

struct tm;

void
timeh (char *s, __SIZE_TYPE__ sz, struct tm *tm)
{
  strftime (s, sz, "", tm); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..time.h.." "" { target *-*-* } .-1 } */
}
