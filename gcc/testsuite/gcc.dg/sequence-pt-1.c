/* Test for sequence point warnings.  */
/* Origin: Michael Meeks in
   <URL:http://gcc.gnu.org/ml/gcc-patches/1998-06/msg00316.html>,
   adapted to a testcase by Joseph Myers <jsm28@cam.ac.uk>.  */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

struct s
{
  int a;
};

extern int fn (int);
extern int fnb (int, int);
extern int fnc (int *);
extern int sprintf (char *, const char *, ...);

void
foo (int a, int b, int n, int p, int *ptr, struct s *sptr,
     int *ap, int *bp, int **cp, char *ans)
{
  int len;
    
  a = a++; /* { dg-warning "undefined" "sequence point warning" } */
  a = --a; /* { dg-warning "undefined" "sequence point warning" } */
  a = ++a + b; /* { dg-warning "undefined" "sequence point warning" } */
  a = a-- + b; /* { dg-warning "undefined" "sequence point warning" } */
  ap[n] = bp[n++]; /* { dg-warning "undefined" "sequence point warning" } */
  ap[--n] = bp[n]; /* { dg-warning "undefined" "sequence point warning" } */
  ap[++n] = bp[--n]; /* { dg-warning "undefined" "sequence point warning" } */
  cp[n][n] = cp[n][n]++; /* { dg-warning "undefined" "sequence point warning" { xfail *-*-* } } */
  cp[n][p] = cp[n][n++]; /* { dg-warning "undefined" "sequence point warning" } */
  *ptr++ = (int)ptr++; /* { dg-warning "undefined" "sequence point warning" } */
  sptr->a = sptr->a++; /* { dg-warning "undefined" "sequence point warning" { xfail *-*-* } } */
  sptr->a = (int)(sptr++); /* { dg-warning "undefined" "sequence point warning" } */
  *ptr++ = fn (*ptr); /* { dg-warning "undefined" "sequence point warning" } */
  a = b = a++; /* { dg-warning "undefined" "sequence point warning" } */
  b = a = --b; /* { dg-warning "undefined" "sequence point warning" } */
  a = 1 + (a = 1); /* { dg-warning "undefined" "sequence point warning" } */
  a = (a = b); /* { dg-warning "undefined" "sequence point warning" } */
  a = (a = b) + 1; /* { dg-warning "undefined" "sequence point warning" } */
  a = (bp[a++] = b) + 1; /* { dg-warning "undefined" "sequence point warning" } */
  a = b++ * b++; /* { dg-warning "undefined" "sequence point warning" } */
  a = fnb (b++, b++); /* { dg-warning "undefined" "sequence point warning" } */
  *ap = fnc (ap++); /* { dg-warning "undefined" "sequence point warning" } */
  (a += b) + (a += n); /* { dg-warning "undefined" "sequence point warning" } */
  a =  (b, b++) + (b++, b); /* { dg-warning "undefined" "sequence point warning" } */

  a = (a++ && 4); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  len = sprintf (ans, "%d", len++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = fn (a++); /* { dg-bogus "undefined" "sequence point warning" } */
  (a = b++), (a = b++); /* { dg-bogus "undefined" "sequence point warning" } */
  a = (b++, b++); /* { dg-bogus "undefined" "sequence point warning" } */
  a = b++ && b++; /* { dg-bogus "undefined" "sequence point warning" } */
  a = b++ || b++; /* { dg-bogus "undefined" "sequence point warning" } */
  a = (b++ ? b++ : a); /* { dg-bogus "undefined" "sequence point warning" } */
  a = (b++ ? a : b++); /* { dg-bogus "undefined" "sequence point warning" } */
  ap[a++] += bp[b]; /* { dg-bogus "undefined" "sequence point warning" } */
}
