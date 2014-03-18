/* { dg-do compile } */
/* { dg-options "-Wcast-qual" } */

void
f1 (void *bar)
{
  const void *p1 = (const void *) bar;
  const char *p2 = (const char *) bar;
  const void **p3 = (const void **) bar;
  const char **p4 = (const char **) bar;
  const void * const *p5 = (const void * const *) bar;
  const char * const *p6 = (const char * const *) bar;
  void * const *p7 = (void * const *) bar;
  char * const *p8 = (char * const *) bar;
  const void ***p9 = (const void ***) bar;
  const char ***p10 = (const char ***) bar;
  void * const **p11 = (void * const **) bar;
  char * const **p12 = (char * const **) bar;
  void ** const *p13 = (void ** const *) bar;
  char ** const *p14 = (char ** const *) bar;
  const void * const **p15 = (const void * const **) bar;
  const char * const **p16 = (const char * const **) bar;
  const void ** const *p17 = (const void ** const *) bar;
  const char ** const *p18 = (const char ** const *) bar;
  void * const * const * p19 = (void * const * const *) bar;
  char * const * const * p20 = (char * const * const *) bar;
  const void * const * const *p21 = (const void * const * const *) bar;
  const char * const * const *p22 = (const char * const * const *) bar;
}

void
f2 (void **bar)
{
  const void *p1 = (const void *) bar;
  const char *p2 = (const char *) bar;
  const void **p3 = (const void **) bar; /* { dg-warning "cast" } */
  const char **p4 = (const char **) bar;
  const void * const *p5 = (const void * const *) bar;
  const char * const *p6 = (const char * const *) bar;
  void * const *p7 = (void * const *) bar;
  char * const *p8 = (char * const *) bar;
  const void ***p9 = (const void ***) bar;
  const char ***p10 = (const char ***) bar;
  void * const **p11 = (void * const **) bar;
  char * const **p12 = (char * const **) bar;
  void ** const *p13 = (void ** const *) bar;
  char ** const *p14 = (char ** const *) bar;
  const void * const **p15 = (const void * const **) bar;
  const char * const **p16 = (const char * const **) bar;
  const void ** const *p17 = (const void ** const *) bar;
  const char ** const *p18 = (const char ** const *) bar;
  void * const * const * p19 = (void * const * const *) bar;
  char * const * const * p20 = (char * const * const *) bar;
  const void * const * const *p21 = (const void * const * const *) bar;
  const char * const * const *p22 = (const char * const * const *) bar;
}

void
f3 (void ***bar)
{
  const void *p1 = (const void *) bar;
  const char *p2 = (const char *) bar;
  const void **p3 = (const void **) bar;
  const char **p4 = (const char **) bar;
  const void * const *p5 = (const void * const *) bar;
  const char * const *p6 = (const char * const *) bar;
  void * const *p7 = (void * const *) bar;
  char * const *p8 = (char * const *) bar;
  const void ***p9 = (const void ***) bar; /* { dg-warning "cast" } */
  const char ***p10 = (const char ***) bar;
  void * const **p11 = (void * const **) bar; /* { dg-warning "cast" } */
  char * const **p12 = (char * const **) bar;
  void ** const *p13 = (void ** const *) bar;
  char ** const *p14 = (char ** const *) bar;
  const void * const **p15 = (const void * const **) bar; /* { dg-warning "cast" } */
  const char * const **p16 = (const char * const **) bar;
  const void ** const *p17 = (const void ** const *) bar; /* { dg-warning "cast" } */
  const char ** const *p18 = (const char ** const *) bar;
  void * const * const * p19 = (void * const * const *) bar;
  char * const * const * p20 = (char * const * const *) bar;
  const void * const * const *p21 = (const void * const * const *) bar;
  const char * const * const *p22 = (const char * const * const *) bar;
}

void
f4 (void * const **bar)
{
  const void ***p9 = (const void ***) bar; /* { dg-warning "cast" } */
  void * const **p11 = (void * const **) bar;
  void ** const *p13 = (void ** const *) bar; /* { dg-warning "cast" } */
  const void * const **p15 = (const void * const **) bar; /* { dg-warning "cast" } */
  const void ** const *p17 = (const void ** const *) bar; /* { dg-warning "cast" } */
  void * const * const * p19 = (void * const * const *) bar;
  const void * const * const *p21 = (const void * const * const *) bar;
}

void
f5 (char ***bar)
{
  volatile const char ***p9 = (volatile const char ***) bar; /* { dg-warning "cast" } */
  volatile char * const **p11 = (volatile char * const **) bar; /* { dg-warning "cast" } */
  volatile char ** const *p13 = (volatile char ** const *) bar; /* { dg-warning "cast" } */
  volatile const char * const **p15 = (volatile const char * const **) bar; /* { dg-warning "cast" } */
  volatile const char ** const *p17 = (volatile const char ** const *) bar; /* { dg-warning "cast" } */
  volatile char * const * const * p19 = (volatile char * const * const *) bar;
  volatile const char * const * const *p21 = (volatile const char * const * const *) bar;
}

void
f6 (char ***bar)
{
  const char * volatile **p9 = (const char * volatile **) bar; /* { dg-warning "cast" } */
  char * volatile const **p11 = (char * volatile const **) bar; /* { dg-warning "cast" } */
  char * volatile * const *p13 = (char * volatile * const *) bar;
  const char * volatile const **p15 = (const char * volatile const **) bar; /* { dg-warning "cast" } */
  const char * volatile * const *p17 = (const char * volatile * const *) bar; /* { dg-warning "cast" } */
  char * volatile const * const * p19 = (char * volatile const * const *) bar;
  const char * volatile const * const *p21 = (const char * volatile const * const *) bar;
}

void
f7 (char ***bar)
{
  const char ** volatile *p9 = (const char ** volatile *) bar; /* { dg-warning "cast" } */
  char * const * volatile *p11 = (char * const * volatile *) bar; /* { dg-warning "cast" } */
  char ** volatile const *p13 = (char ** volatile const *) bar;
  const char * const * volatile *p15 = (const char * const * volatile *) bar; /* { dg-warning "cast" } */
  const char ** volatile const *p17 = (const char ** volatile const *) bar; /* { dg-warning "cast" } */
  char * const * volatile const * p19 = (char * const * volatile const *) bar;
  const char * const * volatile const *p21 = (const char * const * volatile const *) bar;
}

typedef int (intfn) (int);
typedef intfn *pintfn;
typedef const intfn *constfn;

void
f8 (constfn ***bar)
{
  const constfn *p1 = (const constfn *) bar;
  const pintfn *p2 = (const pintfn *) bar;
  const constfn **p3 = (const constfn **) bar;
  const pintfn **p4 = (const pintfn **) bar;
  const constfn * const *p5 = (const constfn * const *) bar;
  const pintfn * const *p6 = (const pintfn * const *) bar;
  constfn * const *p7 = (constfn * const *) bar;
  pintfn * const *p8 = (pintfn * const *) bar;
  const constfn ***p9 = (const constfn ***) bar; /* { dg-warning "cast" } */
  const pintfn ***p10 = (const pintfn ***) bar; /* { dg-warning "cast" } */
  constfn * const **p11 = (constfn * const **) bar; /* { dg-warning "cast" } */
  pintfn * const **p12 = (pintfn * const **) bar; /* { dg-warning "cast" } */
  constfn ** const *p13 = (constfn ** const *) bar;
  pintfn ** const *p14 = (pintfn ** const *) bar;
  const constfn * const **p15 = (const constfn * const **) bar; /* { dg-warning "cast" } */
  const pintfn * const **p16 = (const pintfn * const **) bar; /* { dg-warning "cast" } */
  const constfn ** const *p17 = (const constfn ** const *) bar; /* { dg-warning "cast" } */
  const pintfn ** const *p18 = (const pintfn ** const *) bar; /* { dg-warning "cast" } */
  constfn * const * const * p19 = (constfn * const * const *) bar;
  pintfn * const * const * p20 = (pintfn * const * const *) bar;
  const constfn * const * const *p21 = (const constfn * const * const *) bar;
  const pintfn * const * const *p22 = (const pintfn * const * const *) bar;
}
