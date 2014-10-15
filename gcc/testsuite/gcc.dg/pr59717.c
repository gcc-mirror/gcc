/* PR c/59717 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11 -Wno-implicit-function-declaration" } */

void
math (double d, int *ex, double *dp)
{
  acos (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 8 } */
  acosh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 10 } */
  asin (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 12 } */
  asinh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 14 } */
  atan (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 16 } */
  atanh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 18 } */
  atan2 (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 20 } */
  cbrt (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 22 } */
  ceil (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 24 } */
  copysign (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 26 } */
  cos (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 28 } */
  cosh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 30 } */
  erf (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 32 } */
  erfc (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 34 } */
  exp (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 36 } */
  exp2 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 38 } */
  expm1 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 40 } */
  fabs (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 42 } */
  fdim (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 44 } */
  floor (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 46 } */
  fma (d, d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 48 } */
  fmax (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 50 } */
  fmin (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 52 } */
  fmod (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 54 } */
  frexp (d, ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 56 } */
  hypot (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 58 } */
  /* We don't generate the warning for ilogb.  */
  ilogb (d);
  ldexp (d, *ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 62 } */
  lgamma (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 64 } */
  llrint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 66 } */
  llround (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 68 } */
  log (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 70 } */
  log10 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 72 } */
  log1p (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 74 } */
  log2 (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 76 } */
  logb (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 78 } */
  lrint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 80 } */
  lround (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 82 } */
  modf (d, dp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 84 } */
  nan (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 86 } */
  nearbyint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 88 } */
  nextafter (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 90 } */
  nexttoward (d, 20.0L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 92 } */
  pow (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 94 } */
  remainder (d, d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 96 } */
  remquo (d, d, ex); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 98 } */
  rint (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 100 } */
  round (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 102 } */
  scalbln (d, 100L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 104 } */
  scalbn (d, 100); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 106 } */
  sin (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 108 } */
  sinh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 110 } */
  sincos (d, dp, dp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 112 } */
  sqrt (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 114 } */
  tan (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 116 } */
  tanh (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 118 } */
  tgamma (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 120 } */
  trunc (d); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..math.h.." "" { target *-*-* } 122 } */
}

void
cmplx (double _Complex z)
{
  cabs (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 129 } */
  cacos (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 131 } */
  cacosh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 133 } */
  carg (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 135 } */
  casin (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 137 } */
  casinh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 139 } */
  catan (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 141 } */
  catanh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 143 } */
  ccos (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 145 } */
  ccosh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 147 } */
  cexp (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 149 } */
  cimag (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 151 } */
  clog (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 153 } */
  conj (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 155 } */
  cpow (z, z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 157 } */
  cproj (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 159 } */
  creal (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 161 } */
  csin (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 163 } */
  csinh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 165 } */
  csqrt (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 167 } */
  ctan (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 169 } */
  ctanh (z); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..complex.h.." "" { target *-*-* } 171 } */
}

void
string (void *p, void *q, __SIZE_TYPE__ sz)
{
  memchr (p, 2, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 178 } */
  /* We don't generate the warning for memcmp.  */
  memcmp (p, q, sz);
  memcpy (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 182 } */
  memmove (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 184 } */
  memset (p, 0, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 186 } */
  strcat (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 188 } */
  strchr (p, 'a'); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 190 } */
  /* We don't generate the warning for strcmp.  */
  strcmp (p, q);
  strcpy (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 194 } */
  strcspn (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 196 } */
  strlen (p); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 198 } */
  strncat (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 200 } */
  /* We don't generate the warning for strncmp.  */
  strncmp (p, q, sz);
  strncpy (p, q, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 204 } */
  strpbrk (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 206 } */
  strrchr (p, 'q'); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 208 } */
  strspn (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 210 } */
  strstr (p, q); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..string.h.." "" { target *-*-* } 212 } */
}

/* Fake FILE.  */
typedef struct { int i; } FILE;

void
stdio (FILE *fp, void *p, __SIZE_TYPE__ sz)
{
  fprintf (fp, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 222 } */
  fscanf (fp, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 224 } */
  fwrite (p, sz, sz, fp); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 226 } */
  printf (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 228 } */
  scanf (""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 230 } */
  snprintf ("", sz, ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 232 } */
  sprintf ("", ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 234 } */
  sscanf ("", ""); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdio.h.." "" { target *-*-* } 236 } */
}

void
stdlib (void *p, void *q, __SIZE_TYPE__ sz)
{
  abort (); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 243 } */
  calloc (sz, 1); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 245 } */
  exit (1); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 247 } */
  free (p); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 249 } */
  labs (1L); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 251 } */
  llabs (1LL); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 253 } */
  malloc (sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 255 } */
  realloc (p, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 257 } */
  aligned_alloc (sz, sz); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..stdlib.h.." "" { target *-*-* } 259 } */
}

void
inttypes (__INTMAX_TYPE__ j)
{
  imaxabs (j); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..inttypes.h.." "" { target *-*-* } 266 } */
}

struct tm;

void
timeh (char *s, __SIZE_TYPE__ sz, struct tm *tm)
{
  strftime (s, sz, "", tm); /* { dg-warning "incompatible implicit" } */
  /* { dg-message "include ..time.h.." "" { target *-*-* } 275 } */
}
