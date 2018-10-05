/* PR tree-optimization/86552 - missing warning for reading past the end
   of non-string arrays
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;
extern size_t strnlen (const char*, size_t);

const char a[5] = "12345";   /* { dg-message "declared here" } */
enum { asz = sizeof a };

int v0 = 0;
int v1 = 1;

void sink (int, ...);

#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT(a, b)

#define T(str, n)					\
  __attribute__ ((noipa))				\
  void CAT (test_, __LINE__) (void) {			\
    int i0 = 0, i1 = i0 + 1, i2 = i1 + 1, i3 = i2 + 1;	\
    sink (strnlen (str, n), i0, i1, i2, i3);		\
  } typedef void dummy_type

T (a, asz);
T (a, asz - 1);
T (a, asz - 2);
T (a, asz - 5);
T (&a[0], asz);
T (&a[0] + 1, asz);            /* { dg-warning "specified bound 5 exceeds the size 4 of unterminated array" } */
T (&a[1], asz);                /* { dg-warning "specified bound 5 exceeds the size 4 of unterminated array" } */
T (&a[1], asz - 1);
T (&a[v0], asz);               /* { dg-warning "specified bound 5 may exceed the size of at most 5 of unterminated array" } */
T (&a[v0] + 1, asz);           /* { dg-warning "specified bound 5 may exceed the size of at most 5 of unterminated array" } */

T (a, asz + 1);                /* { dg-warning "specified bound 6 exceeds the size 5 " } */
T (&a[0], asz + 1);            /* { dg-warning "unterminated" } */
T (&a[0] + 1, asz - 1);
T (&a[0] + 1, asz + 1);        /* { dg-warning "unterminated" } */
T (&a[1], asz + 1);            /* { dg-warning "unterminated" } */
T (&a[v0], asz + 1);           /* { dg-warning "unterminated" } */
T (&a[v0] + 1, asz + 1);       /* { dg-warning "unterminated" } */


const char b[][5] = { /* { dg-message "declared here" } */
  "12", "123", "1234", "54321"
};
enum { bsz = sizeof b[0] };

T (b[0], bsz);
T (b[1], bsz);
T (b[2], bsz);
T (b[3], bsz);

T (b[0], bsz - 1);
T (b[1], bsz - 1);
T (b[2], bsz - 1);
T (b[3], bsz - 1);

T (b[0], bsz + 1);
T (b[1], bsz + 1);
T (b[2], bsz + 1);
T (b[3], bsz + 1);            /* { dg-warning "unterminated" } */

T (b[i0], bsz);
T (b[i1], bsz);
T (b[i2], bsz);
T (b[i3], bsz);

T (b[i0], bsz + 1);
T (b[i1], bsz + 1);
T (b[i2], bsz + 1);
T (b[i3], bsz + 1);           /* { dg-warning "unterminated" } */

T (b[v0], bsz);
T (b[v0], bsz + 1);

T (&b[i2][i1], bsz);
T (&b[i2][i1] + i1, bsz);
T (&b[i2][v0], bsz);
T (&b[i2][i1] + v0, bsz);

T (&b[i2][i1], bsz + 1);
T (&b[i2][i1] + i1, bsz + 1);
T (&b[i2][v0], bsz + 1);
T (&b[i2][i1] + v0, bsz + 1);

T (&b[2][1], bsz);
T (&b[2][1] + i1, bsz);
T (&b[2][i0], bsz);
T (&b[2][1] + i0, bsz);
T (&b[2][1] + v0, bsz);
T (&b[2][v0], bsz);

T (&b[2][1], bsz + 1);
T (&b[2][1] + i1, bsz + 1);
T (&b[2][i0], bsz + 1);
T (&b[2][1] + i0, bsz + 1);
T (&b[2][1] + v0, bsz + 1);
T (&b[2][v0], bsz + 1);

T (&b[3][1], bsz);                /* { dg-warning "unterminated" } */
T (&b[3][1], bsz - 1);
T (&b[3][1] + 1, bsz);            /* { dg-warning "unterminated" } */
T (&b[3][1] + 1, bsz - 1);        /* { dg-warning "unterminated" } */
T (&b[3][1] + 1, bsz - 2);
T (&b[3][1] + i1, bsz);           /* { dg-warning "unterminated" } */
T (&b[3][1] + i1, bsz - i1);      /* { dg-warning "unterminated" } */
T (&b[3][1] + i1, bsz - i2);
T (&b[3][v0], bsz);
T (&b[3][1] + v0, bsz);           /* { dg-warning "specified bound 5 may exceed the size of at most 4 of unterminated array" } */
T (&b[3][v0] + v1, bsz);          /* { dg-warning "specified bound 5 may exceed the size of at most 4 of unterminated array" "pr?????" { xfail *-*-* } } */

T (&b[3][1], bsz + 1);            /* { dg-warning "unterminated" } */
T (&b[3][1] + 1, bsz + 1);        /* { dg-warning "unterminated" } */
T (&b[3][1] + i1, bsz + 1);       /* { dg-warning "unterminated" } */
T (&b[3][v0], bsz + 1);           /* { dg-warning "unterminated" "pr86936" { xfail *-*-* } } */
T (&b[3][1] + v0, bsz + 1);       /* { dg-warning "unterminated" } */
T (&b[3][v0] + v1, bsz + 1);      /* { dg-warning "unterminated" "pr86936" { xfail *-*-* } } */

T (&b[i3][i1], bsz);              /* { dg-warning "unterminated" } */
T (&b[i3][i1] + 1, bsz);          /* { dg-warning "unterminated" } */
T (&b[i3][i1] + i1, bsz);         /* { dg-warning "specified bound 5 exceeds the size 3 of unterminated array" } */
T (&b[i3][v0], bsz);
T (&b[i3][i1] + v0, bsz);         /* { dg-warning "specified bound 5 may exceed the size of at most 4 of unterminated array" } */
T (&b[i3][v0] + v1, bsz);

T (&b[i3][i1], bsz + 1);          /* { dg-warning "unterminated" } */
T (&b[i3][i1] + 1, bsz + 1);      /* { dg-warning "unterminated" } */
T (&b[i3][i1] + i1, bsz + 1);     /* { dg-warning "unterminated" } */
T (&b[i3][v0], bsz + 1);          /* { dg-warning "unterminated" "pr86919" { xfail *-*-* } } */
T (&b[i3][i1] + v0, bsz + 1);     /* { dg-warning "unterminated" } */
T (&b[i3][v0] + v1, bsz + 1);     /* { dg-warning "unterminated" "pr86919" { xfail *-*-* } } */

T (v0 ? "" : b[0], bsz);
T (v0 ? "" : b[1], bsz);
T (v0 ? "" : b[2], bsz);
T (v0 ? "" : b[3], bsz);
T (v0 ? b[0] : "", bsz);
T (v0 ? b[1] : "", bsz);
T (v0 ? b[2] : "", bsz);
T (v0 ? b[3] : "", bsz);

T (v0 ? "" : b[0], bsz + 1);
T (v0 ? "" : b[1], bsz + 1);
T (v0 ? "" : b[2], bsz + 1);
T (v0 ? "" : b[3], bsz + 1);      /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[0] : "", bsz + 1);
T (v0 ? b[1] : "", bsz + 1);
T (v0 ? b[2] : "", bsz + 1);
T (v0 ? b[3] : "", bsz + 1);      /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */

T (v0 ? "" : b[i0], bsz);
T (v0 ? "" : b[i1], bsz);
T (v0 ? "" : b[i2], bsz);
T (v0 ? "" : b[i3], bsz);
T (v0 ? b[i0] : "", bsz);
T (v0 ? b[i1] : "", bsz);
T (v0 ? b[i2] : "", bsz);
T (v0 ? b[i3] : "", bsz);

T (v0 ? "" : b[i0], bsz + 1);
T (v0 ? "" : b[i1], bsz + 1);
T (v0 ? "" : b[i2], bsz + 1);
T (v0 ? "" : b[i3], bsz + 1);     /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[i0] : "", bsz + 1);
T (v0 ? b[i1] : "", bsz + 1);
T (v0 ? b[i2] : "", bsz + 1);
T (v0 ? b[i3] : "", bsz + 1);     /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */

T (v0 ? "1234" : b[3], bsz);
T (v0 ? "1234" : b[i3], bsz);
T (v0 ? b[3] : "1234", bsz);
T (v0 ? b[i3] : "1234", bsz);

T (v0 ? a : b[3], bsz);
T (v0 ? b[0] : b[2], bsz);
T (v0 ? b[2] : b[3], bsz);
T (v0 ? b[3] : b[2], bsz);

T (v0 ? "1234" : b[3], bsz + 1);  /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? "1234" : b[i3], bsz + 1); /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[3] : "1234", bsz + 1);  /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[i3] : "1234", bsz + 1); /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */

T (v0 ? a : b[3], bsz + 1);       /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[0] : b[2], bsz + 1);
T (v0 ? b[2] : b[3], bsz + 1);    /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */
T (v0 ? b[3] : b[2], bsz + 1);    /* { dg-warning "unterminated" "pr86937" { xfail *-*-* } } */

struct A { char a[5], b[5]; };

const struct A s = { "1234", "12345" };

T (s.a, asz);
T (&s.a[0], asz);
T (&s.a[0] + 1, asz);
T (&s.a[0] + v0, asz);
T (&s.a[1], asz);
T (&s.a[1] + 1, asz);
T (&s.a[1] + v0, asz);

T (&s.a[i0], asz);
T (&s.a[i0] + i1, asz);
T (&s.a[i0] + v0, asz);
T (&s.a[i1], asz);
T (&s.a[i1] + i1, asz);
T (&s.a[i1] + v0, asz);

T (s.a, asz + 1);
T (&s.a[0], asz + 1);
T (&s.a[0] + 1, asz + 1);
T (&s.a[0] + v0, asz + 1);
T (&s.a[1], asz + 1);
T (&s.a[1] + 1, asz + 1);
T (&s.a[1] + v0, asz + 1);

T (&s.a[i0], asz + 1);
T (&s.a[i0] + i1, asz + 1);
T (&s.a[i0] + v0, asz + 1);
T (&s.a[i1], asz + 1);
T (&s.a[i1] + i1, asz + 1);
T (&s.a[i1] + v0, asz + 1);

T (s.b, bsz);
T (&s.b[0], bsz);
T (&s.b[0] + 1, bsz);             /* { dg-warning "unterminated" } */
T (&s.b[0] + v0, bsz);            /* { dg-warning "unterminated" } */
T (&s.b[1], bsz);                 /* { dg-warning "unterminated" } */
T (&s.b[1] + 1, bsz);             /* { dg-warning "unterminated" } */
T (&s.b[1] + v0, bsz);            /* { dg-warning "unterminated" } */

T (&s.b[i0], bsz);
T (&s.b[i0] + i1, bsz);           /* { dg-warning "unterminated" } */
T (&s.b[i0] + v0, bsz);           /* { dg-warning "unterminated" } */
T (&s.b[i1], bsz);                /* { dg-warning "unterminated" } */
T (&s.b[i1] + i1, bsz);           /* { dg-warning "unterminated" } */
T (&s.b[i1] + v0, bsz);           /* { dg-warning "unterminated" } */

T (s.b, bsz + 1);                 /* { dg-warning "unterminated" } */
T (&s.b[0], bsz + 1);             /* { dg-warning "unterminated" } */
T (&s.b[0] + 1, bsz + 1);         /* { dg-warning "unterminated" } */
T (&s.b[0] + v0, bsz + 1);        /* { dg-warning "unterminated" } */
T (&s.b[1], bsz + 1);             /* { dg-warning "unterminated" } */
T (&s.b[1] + 1, bsz + 1);         /* { dg-warning "unterminated" } */
T (&s.b[1] + v0, bsz + 1);        /* { dg-warning "unterminated" } */

T (&s.b[i0], bsz + 1);            /* { dg-warning "unterminated" } */
T (&s.b[i0] + i1, bsz + 1);       /* { dg-warning "unterminated" } */
T (&s.b[i0] + v0, bsz + 1);       /* { dg-warning "unterminated" } */
T (&s.b[i1], bsz + 1);            /* { dg-warning "unterminated" } */
T (&s.b[i1] + i1, bsz + 1);       /* { dg-warning "unterminated" } */
T (&s.b[i1] + v0, bsz + 1);       /* { dg-warning "unterminated" } */

struct B { struct A a[2]; };

const struct B ba[] = {
  { { { "123", "12345" }, { "12345", "123" } } },
  { { { "12345", "123" }, { "123", "12345" } } },
  { { { "1", "12" },      { "123", "1234" } } },
  { { { "123", "1234" },  { "12345", "12" } } }
};

T (ba[0].a[0].a, asz + 1);
T (&ba[0].a[0].a[0], asz + 1);
T (&ba[0].a[0].a[0] + 1, asz + 1);
T (&ba[0].a[0].a[0] + v0, asz + 1);
T (&ba[0].a[0].a[1], asz + 1);
T (&ba[0].a[0].a[1] + 1, asz + 1);
T (&ba[0].a[0].a[1] + v0, asz + 1);

T (ba[0].a[0].b, bsz);
T (&ba[0].a[0].b[0], bsz);
T (&ba[0].a[0].b[0] + 1, bsz);        /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[0] + 1, bsz - 1);
T (&ba[0].a[0].b[0] + v0, bsz);       /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1], bsz);            /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1], bsz - 1);
T (&ba[0].a[0].b[1] + 1, bsz - 1);    /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1] + 1, bsz - 2);
T (&ba[0].a[0].b[1] + 1, bsz);        /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1] + v0, bsz);       /* { dg-warning "unterminated" } */

T (ba[0].a[0].b, bsz + 1);            /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[0], bsz + 1);        /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[0] + 1, bsz + 1);    /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[0] + v0, bsz + 1);   /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1], bsz + 1);        /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1] + 1, bsz + 1);    /* { dg-warning "unterminated" } */
T (&ba[0].a[0].b[1] + v0, bsz + 1);   /* { dg-warning "unterminated" } */

T (ba[0].a[1].a, asz + 1);            /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[0], asz + 1);        /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[0] + 1, asz + 1);    /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[0] + v0, asz + 1);   /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[1], asz + 1);        /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[1] + 1, asz + 1);    /* { dg-warning "unterminated" } */
T (&ba[0].a[1].a[1] + v0, asz + 1);   /* { dg-warning "unterminated" } */

T (ba[0].a[1].b, bsz + 1);
T (&ba[0].a[1].b[0], bsz + 1);
T (&ba[0].a[1].b[0] + 1, bsz + 1);
T (&ba[0].a[1].b[0] + v0, bsz + 1);
T (&ba[0].a[1].b[1], bsz + 1);
T (&ba[0].a[1].b[1] + 1, bsz + 1);
T (&ba[0].a[1].b[1] + v0, bsz + 1);

T (ba[1].a[0].a, asz);
T (&ba[1].a[0].a[0], asz);
T (&ba[1].a[0].a[0] + 1, asz);        /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[0] + v0, asz);       /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1], asz);            /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1] + 1, asz);        /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1] + v0, asz);       /* { dg-warning "unterminated" } */

T (ba[1].a[0].a, asz + 1);            /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[0], asz + 1);        /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[0] + 1, asz + 1);    /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[0] + v0, asz + 1);   /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1], asz + 1);        /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1] + 1, asz + 1);    /* { dg-warning "unterminated" } */
T (&ba[1].a[0].a[1] + v0, asz + 1);   /* { dg-warning "unterminated" } */

T (ba[1].a[0].b, bsz);
T (&ba[1].a[0].b[0], bsz);
T (&ba[1].a[0].b[0] + 1, bsz);
T (&ba[1].a[0].b[0] + v0, bsz);
T (&ba[1].a[0].b[1], bsz);
T (&ba[1].a[0].b[1] + 1, bsz);
T (&ba[1].a[0].b[1] + v0, bsz);

T (ba[1].a[1].a, asz);
T (&ba[1].a[1].a[0], asz);
T (&ba[1].a[1].a[0] + 1, asz);
T (&ba[1].a[1].a[0] + v0, asz);
T (&ba[1].a[1].a[1], asz);
T (&ba[1].a[1].a[1] + 1, asz);
T (&ba[1].a[1].a[1] + v0, asz);

T (ba[1].a[1].b, bsz);
T (&ba[1].a[1].b[0], bsz);
T (&ba[1].a[1].b[0] + 1, bsz);        /* { dg-warning "unterminated" } */
T (&ba[1].a[1].b[0] + 1, bsz - 1);
T (&ba[1].a[1].b[0] + v0, bsz);       /* { dg-warning "unterminated" } */
T (&ba[1].a[1].b[1], bsz);            /* { dg-warning "unterminated" } */
T (&ba[1].a[1].b[1], bsz - 1);
T (&ba[1].a[1].b[1] + 1, bsz);        /* { dg-warning "unterminated" } */
T (&ba[1].a[1].b[1] + 1, bsz - 1);    /* { dg-warning "unterminated" } */
T (&ba[1].a[1].b[1] + 1, bsz - 2);
T (&ba[1].a[1].b[1] + 1, bsz - i2);
T (&ba[1].a[1].b[1] + v0, bsz);       /* { dg-warning "unterminated" } */

/* Prune out warnings with no location (pr?????).
   { dg-prune-output "cc1:" } */
