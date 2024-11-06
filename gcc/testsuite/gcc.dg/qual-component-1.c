/* Test qualification of components of qualified structures or unions:
   should have qualifiers from both the component and the structure or
   union.  Bug 27697 from Frank Victor Fischer.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -pedantic -Wdiscarded-array-qualifiers" } */

struct s {
  int a;
  int b[1];
  int c[2][3];
  const int d;
  const int e[1];
  const int f[2][3];
};

union u {
  int a;
  int b[1];
  int c[2][3];
  const int d;
  const int e[1];
  const int f[2][3];
};

struct cs {
  const struct s x;
};

struct s v1;
union u *v2;
const struct s *v3;
const union u v4;
struct cs v5;

void
f (void)
{
  v1.a = 0;
  v1.b[0] = 0;
  *v1.b = 0;
  v1.c[0][0] = 0;
  *v1.c[0] = 0;
  **v1.c = 0;
  v1.d = 0; /* { dg-error "assignment of read-only" } */
  v1.e[0] = 0; /* { dg-error "assignment of read-only" } */
  *v1.e = 0; /* { dg-error "assignment of read-only" } */
  v1.f[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v1.f[0] = 0; /* { dg-error "assignment of read-only" } */
  **v1.f = 0; /* { dg-error "assignment of read-only" } */

  v2->a = 0;
  v2->b[0] = 0;
  *v2->b = 0;
  v2->c[0][0] = 0;
  *v2->c[0] = 0;
  **v2->c = 0;
  v2->d = 0; /* { dg-error "assignment of read-only" } */
  v2->e[0] = 0; /* { dg-error "assignment of read-only" } */
  *v2->e = 0; /* { dg-error "assignment of read-only" } */
  v2->f[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v2->f[0] = 0; /* { dg-error "assignment of read-only" } */
  **v2->f = 0; /* { dg-error "assignment of read-only" } */

  v3->a = 0; /* { dg-error "assignment of member" } */
  v3->b[0] = 0; /* { dg-error "assignment of read-only" } */
  *v3->b = 0; /* { dg-error "assignment of read-only" } */
  v3->c[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v3->c[0] = 0; /* { dg-error "assignment of read-only" } */
  **v3->c = 0; /* { dg-error "assignment of read-only" } */
  v3->d = 0; /* { dg-error "assignment of member" } */
  v3->e[0] = 0; /* { dg-error "assignment of read-only" } */
  *v3->e = 0; /* { dg-error "assignment of read-only" } */
  v3->f[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v3->f[0] = 0; /* { dg-error "assignment of read-only" } */
  **v3->f = 0; /* { dg-error "assignment of read-only" } */

  v4.a = 0; /* { dg-error "assignment of member" } */
  v4.b[0] = 0; /* { dg-error "assignment of read-only" } */
  *v4.b = 0; /* { dg-error "assignment of read-only" } */
  v4.c[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v4.c[0] = 0; /* { dg-error "assignment of read-only" } */
  **v4.c = 0; /* { dg-error "assignment of read-only" } */
  v4.d = 0; /* { dg-error "assignment of member" } */
  v4.e[0] = 0; /* { dg-error "assignment of read-only" } */
  *v4.e = 0; /* { dg-error "assignment of read-only" } */
  v4.f[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v4.f[0] = 0; /* { dg-error "assignment of read-only" } */
  **v4.f = 0; /* { dg-error "assignment of read-only" } */

  v5.x.a = 0; /* { dg-error "assignment of member" } */
  v5.x.b[0] = 0; /* { dg-error "assignment of read-only" } */
  *v5.x.b = 0; /* { dg-error "assignment of read-only" } */
  v5.x.c[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v5.x.c[0] = 0; /* { dg-error "assignment of read-only" } */
  **v5.x.c = 0; /* { dg-error "assignment of read-only" } */
  v5.x.d = 0; /* { dg-error "assignment of member" } */
  v5.x.e[0] = 0; /* { dg-error "assignment of read-only" } */
  *v5.x.e = 0; /* { dg-error "assignment of read-only" } */
  v5.x.f[0][0] = 0; /* { dg-error "assignment of read-only" } */
  *v5.x.f[0] = 0; /* { dg-error "assignment of read-only" } */
  **v5.x.f = 0; /* { dg-error "assignment of read-only" } */
}

void
g (void)
{
  {
    int *a = &v1.a;
    int (*b)[1] = &v1.b;
    int (*c)[2][3] = &v1.c;
    int (*cc)[3] = v1.c;
    const int (*ff)[3] = v1.c; /* { dg-warning "pointers to arrays with different qualifiers" } */
    a = &v1.a;
    b = &v1.b;
    c = &v1.c;
    cc = v1.c;
    ff = v1.c; /* { dg-warning "pointers to arrays with different qualifiers" } */
  }
  {
    const int *d = &v1.d;
    const int (*e)[1] = &v1.e;
    const int (*f)[2][3] = &v1.f;
    const int (*ff)[3] = v1.f;
    int (*cc)[3] = v1.f; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v1.d;
    e = &v1.e;
    f = &v1.f;
    ff = v1.f;
    cc = v1.f; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }

  {
    int *a = &v2->a;
    int (*b)[1] = &v2->b;
    int (*c)[2][3] = &v2->c;
    int (*cc)[3] = v2->c;
    const int (*ff)[3] = v2->c; /* { dg-warning "pointers to arrays with different qualifiers" } */
    a = &v2->a;
    b = &v2->b;
    c = &v2->c;
    cc = v2->c;
    ff = v2->c; /* { dg-warning "pointers to arrays with different qualifiers" } */
  }
  {
    const int *d = &v2->d;
    const int (*e)[1] = &v2->e;
    const int (*f)[2][3] = &v2->f;
    const int (*ff)[3] = v2->f;
    int (*cc)[3] = v2->f; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v2->d;
    e = &v2->e;
    f = &v2->f;
    ff = v2->f;
    cc = v2->f; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }

  {
    const int *d = &v3->a;
    const int (*e)[1] = &v3->b;
    const int (*f)[2][3] = &v3->c;
    const int (*ff)[3] = v3->c;
    int (*cc)[3] = v3->c; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v3->a;
    e = &v3->b;
    f = &v3->c;
    ff = v3->c;
    cc = v3->c; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }
  {
    const int *d = &v3->d;
    const int (*e)[1] = &v3->e;
    const int (*f)[2][3] = &v3->f;
    const int (*ff)[3] = v3->f;
    int (*cc)[3] = v3->f; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v3->d;
    e = &v3->e;
    f = &v3->f;
    ff = v3->f;
    cc = v3->f; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }

  {
    const int *d = &v4.a;
    const int (*e)[1] = &v4.b;
    const int (*f)[2][3] = &v4.c;
    const int (*ff)[3] = v4.c;
    int (*cc)[3] = v4.c; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v4.a;
    e = &v4.b;
    f = &v4.c;
    ff = v4.c;
    cc = v4.c; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }
  {
    const int *d = &v4.d;
    const int (*e)[1] = &v4.e;
    const int (*f)[2][3] = &v4.f;
    const int (*ff)[3] = v4.f;
    int (*cc)[3] = v4.f; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v4.d;
    e = &v4.e;
    f = &v4.f;
    ff = v4.f;
    cc = v4.f; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }

  {
    const int *d = &v5.x.a;
    const int (*e)[1] = &v5.x.b;
    const int (*f)[2][3] = &v5.x.c;
    const int (*ff)[3] = v5.x.c;
    int (*cc)[3] = v5.x.c; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v5.x.a;
    e = &v5.x.b;
    f = &v5.x.c;
    ff = v5.x.c;
    cc = v5.x.c; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }
  {
    const int *d = &v5.x.d;
    const int (*e)[1] = &v5.x.e;
    const int (*f)[2][3] = &v5.x.f;
    const int (*ff)[3] = v5.x.f;
    int (*cc)[3] = v5.x.f; /* { dg-warning "pointers to arrays with different qualifiers|initialization discards" } */
    d = &v5.x.d;
    e = &v5.x.e;
    f = &v5.x.f;
    ff = v5.x.f;
    cc = v5.x.f; /* { dg-warning "pointers to arrays with different qualifiers|assignment discards" } */
  }
}
