/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a[][10], a2[][10];
int b[10], c[10][2], d[10], e[10], f[10];
int b2[10], c2[10][2], d2[10], e2[10], f2[10];
int k[10], l[10], m[10], n[10], o;
int *p;
void bar (void);
int t[10];
#pragma omp threadprivate (t)

void
foo (int g[3][10], int h[4][8], int i[2][10], int j[][9],
     int g2[3][10], int h2[4][8], int i2[2][10], int j2[][9])
{
  #pragma omp task affinity( bar[2:5]) /* { dg-error "is not a variable" } */
    ;
  #pragma omp task affinity( t[2:5])
    ;
  #pragma omp task affinity( k[0.5:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task affinity( l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task affinity( m[p:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task affinity( n[:p]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task affinity( o[2:5]) /* { dg-error "does not have pointer or array type" } */
    ;
  #pragma omp task affinity( a[:][2:4]) /* { dg-error "array type length expression must be specified" } */
    ;
  #pragma omp task affinity( b[-1:]) /* { dg-error "negative low bound in array section" } */
    ;
  #pragma omp task affinity( c[:-3][1:1]) /* { dg-error "negative length in array section" } */
    ;
  #pragma omp task affinity( d[11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( e[:11]) /* { dg-error "length \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( f[1:10]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( g[:][2:4]) /* { dg-error "for array function parameter length expression must be specified" } */
    ;
  #pragma omp task affinity( h[2:2][-1:]) /* { dg-error "negative low bound in array section" } */
    ;
  #pragma omp task affinity( h[:1][:-3]) /* { dg-error "negative length in array section" } */
    ;
  #pragma omp task affinity( i[:1][11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( j[3:4][:10]) /* { dg-error "length \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( j[30:10][5:5]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task affinity( a2[:3][2:4])
    ;
  #pragma omp task affinity( b2[0:])
    ;
  #pragma omp task affinity( c2[:3][1:1])
    ;
  #pragma omp task affinity( d2[9:])
    ;
  #pragma omp task affinity( e2[:10])
    ;
  #pragma omp task affinity( f2[1:9])
    ;
  #pragma omp task affinity( g2[:2][2:4])
    ;
  #pragma omp task affinity( h2[2:2][0:])
    ;
  #pragma omp task affinity( h2[:1][:3])
    ;
  #pragma omp task affinity( i2[:1][9:])
    ;
  #pragma omp task affinity( j2[3:4][:9])
    ;
  #pragma omp task affinity( j2[30:10][5:4])
    ;
}

void bar2 (int a[10][10][10]);

void
foo2 (int a[10][10][10], int **b)
{
  int c[10][10][10];
  #pragma omp task affinity( a[2:4][3:][:7], b[1:7][2:8])
    bar2 (a);
  int i = 1, j = 3, k = 2, l = 6;
  #pragma omp task affinity( a[++i:++j][++k:][:++l])
    bar2 (a);
  #pragma omp task affinity( a[7:2][:][:], c[5:2][:][:])
  {
    bar2 (c);
    bar2 (a);
  }
}

void
foo3 (int a[10][10][10], int **b, int x)
{
  int c[10][10][10];
  #pragma omp task affinity( a[2:4][3:0][:7])	/* { dg-error "zero length array section" } */
    bar2 (a);
  #pragma omp task affinity( b[:7][0:0][:0]) /* { dg-error "zero length array section" } */
    bar2 (a);
  #pragma omp task affinity( c[:][:][10:])	/* { dg-error "zero length array section" } */
    bar2 (c);
  #pragma omp task affinity( a[2:4][3:0][:x])	/* { dg-error "zero length array section" } */
    bar2 (a);
  #pragma omp task affinity( b[:x][0:0][:0]) /* { dg-error "zero length array section" } */
    bar2 (a);
  #pragma omp task affinity( c[:][x-2:x][10:])	/* { dg-error "zero length array section" } */
    bar2 (c);
}

void
foo4 (int *p, int (*q)[10], int r[10], int s[10][10])
{
  int a[10], b[10][10];
  #pragma omp task affinity ( p[-1:2])
  ;
  #pragma omp task affinity ( q[-1:2][2:4])
  ;
  #pragma omp task affinity ( q[-1:2][-2:4]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task affinity ( r[-1:2])
  ;
  #pragma omp task affinity ( s[-1:2][2:4])
  ;
  #pragma omp task affinity ( s[-1:2][-2:4]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task affinity ( a[-1:2])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task affinity ( b[-1:2][2:4])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task affinity ( b[1:2][-2:4])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task affinity ( p[2:-3])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( q[2:-3][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( q[2:3][0:-1])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( r[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( s[2:-5][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( s[2:5][0:-4])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( a[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( b[2:-5][0:10]) /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task affinity ( b[2:5][0:-4]) /* { dg-error "negative length in array section in" } */
  ;
}

struct T { int c[3]; };
struct S { int a; struct T *b; struct T g; };
struct S sd[10];
struct S *se[10];
struct S *sf;
struct S sh;
struct U { int a : 5; };
struct U si;


void
foo5 (void)
{
  #pragma omp task affinity( sd)
  ;
  #pragma omp task affinity( sd[2])
  ;
  #pragma omp task affinity( sd[:])
  ;
  #pragma omp task affinity( sd[2:2])
  ;
  #pragma omp task affinity( sd[:2])
  ;
  #pragma omp task affinity( sd[1].b->c[2])
  ;
  #pragma omp task affinity( sd[0].a)
  ;
  #pragma omp task affinity( se[3]->a)
  ;
  #pragma omp task affinity( se[2]->b->c)
  ;
  #pragma omp task affinity( se[1]->b->c[2])
  ;
  #pragma omp task affinity( (*sf).a)
  ;
  #pragma omp task affinity( sf->b->c[0])
  ;
  #pragma omp task affinity( sf)
  ;
  #pragma omp task affinity( *sf)
  ;
  #pragma omp task affinity( sf[0])
  ;
  #pragma omp task affinity( sf[0].a)
  ;
  #pragma omp task affinity( sh.g.c[2])
  ;
}

void
foo6 (void)
{
  #pragma omp task affinity( sd[:2].b->c[2])	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( sd[1:].b->c[2])	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( sd[0:1].a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( se[3:2]->a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( se[2:2]->b->c)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( se[1]->b->c[2:1])	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( sf + 0)		/* { dg-error "'sf' is not lvalue expression nor array section in 'affinity' clause" } */
  ;
  #pragma omp task affinity( sf[0:1].a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( sh.g.c[2:1])	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity( si.a)		/* { dg-error "bit-field 'si\\..*a' in 'affinity' clause" } */
  ;
}
/* { dg-additional-options "-Wno-volatile" { target c++ } } */
