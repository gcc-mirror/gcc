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

typedef __SIZE_TYPE__ size_t;

void
foo (int a, int b, int n, int p, int *ptr, struct s *sptr,
     int *ap, int *bp, int **cp, char *ans, int (*fnp[8])(int))
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
  *ptr++ = (size_t)ptr++; /* { dg-warning "undefined" "sequence point warning" } */
  sptr->a = sptr->a++; /* { dg-warning "undefined" "sequence point warning" { xfail *-*-* } } */
  sptr->a = (size_t)(sptr++); /* { dg-warning "undefined" "sequence point warning" } */
  *ptr++ = fn (*ptr); /* { dg-warning "undefined" "sequence point warning" } */
  a = b = a++; /* { dg-warning "undefined" "sequence point warning" } */
  b = a = --b; /* { dg-warning "undefined" "sequence point warning" } */
  a = 1 + (a = 1); /* { dg-warning "undefined" "sequence point warning" } */
  a = (a = b); /* { dg-warning "undefined" "sequence point warning" } */
  a = (a = b) + 1; /* { dg-warning "undefined" "sequence point warning" } */
  a = (bp[a++] = b) + 1; /* { dg-warning "undefined" "sequence point warning" } */
  a = b++ * b++; /* { dg-warning "undefined" "sequence point warning" } */
  a = fnb (b++, b++); /* { dg-warning "undefined" "sequence point warning" } */
  a = (*fnp[b++]) (b++); /* { dg-warning "undefined" "sequence point warning" } */
  a = (*fnp[b]) (b++); /* { dg-warning "undefined" "sequence point warning" } */
  a = (*fnp[b++]) (b); /* { dg-warning "undefined" "sequence point warning" } */
  *ap = fnc (ap++); /* { dg-warning "undefined" "sequence point warning" } */
  (a += b) + (a += n); /* { dg-warning "undefined" "sequence point warning" } */
  a =  (b, b++) + (b++, b); /* { dg-warning "undefined" "sequence point warning" } */
  ap[a++] += a; /* { dg-warning "undefined" "sequence point warning" } */
  ap[a+=1] += a; /* { dg-warning "undefined" "sequence point warning" } */
  ap[a++] += a++; /* { dg-warning "undefined" "sequence point warning" } */
  ap[a+=1] += a++; /* { dg-warning "undefined" "sequence point warning" } */
  a = a++, b = a; /* { dg-warning "undefined" "sequence point warning" } */
  b = a, a = a++; /* { dg-warning "undefined" "sequence point warning" } */
  a = (b++ ? n : a) + b; /* { dg-warning "undefined" "sequence point warning" { xfail *-*-* } } */
  b ? a = a++ : a; /* { dg-warning "undefined" "sequence point warning" } */
  b ? a : a = a++; /* { dg-warning "undefined" "sequence point warning" } */
  b && (a = a++); /* { dg-warning "undefined" "sequence point warning" } */
  (a = a++) && b; /* { dg-warning "undefined" "sequence point warning" } */
  b, (a = a++); /* { dg-warning "undefined" "sequence point warning" } */
  (a = a++), b; /* { dg-warning "undefined" "sequence point warning" } */
  a ^= b ^= a ^= b; /* { dg-warning "undefined" "sequence point warning" } */

  a = a; /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = (a++ && 4); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = ! (a++ && 4); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = - (a++ && 4); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = (double) (a++ && 4); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  len = sprintf (ans, "%d", len++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = fn (a++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  b++, (b + b); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  (a = b++), (a = b++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = (b++, b++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = b++ && b++; /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = b++ || b++; /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = (b++ ? b++ : a); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  a = (b++ ? a : b++); /* { dg-bogus "undefined" "bogus sequence point warning" } */
  ap[a++] += bp[b]; /* { dg-bogus "undefined" "bogus sequence point warning" } */
  ap[a += 1] += 1; /* { dg-bogus "undefined" "bogus sequence point warning" } */
  *ptr < 128 ? *ptr++ : *(ptr += 2); /* { dg-bogus "undefined" "bogus sequence point warning" } */

  /* The following will be represented internally with a tree consisting of
     many duplicated SAVE_EXPRs.  This caused the previous version of the
     sequence point warning code to fail by running out of virtual memory.  */
  a = ((b & 1 ? 21 : 0)
       | (b & 2 ? 22 : 0)
       | (b & 3 ? 23 : 0)
       | (b & 4 ? 24 : 0)
       | (b & 5 ? 25 : 0)
       | (b & 6 ? 26 : 0)
       | (b & 7 ? 27 : 0)
       | (b & 8 ? 28 : 0)
       | (b & 9 ? 29 : 0)
       | (b & 10 ? 30 : 0)
       | (b & 11 ? 31 : 0)
       | (b & 12 ? 32 : 0)
       | (b & 13 ? 1 : 0)
       | (b & 14 ? 2 : 0)
       | (b & 15 ? 3 : 0)
       | (b & 16 ? 4 : 0)
       | (b & 17 ? 5 : 0)
       | (b & 18 ? 6 : 0)
       | (b & 19 ? 7 : 0)
       | (b & 20 ? 8 : 0)
       | (b & 21 ? 9 : 0));
}
