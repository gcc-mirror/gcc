/* PR tree-optimization/86552 - missing warning for reading past the end
   of non-string arrays
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

extern __SIZE_TYPE__ strlen (const char*);

const char a[5] = "12345";   /* { dg-message "declared here" } */

int v0 = 0;
int v1 = 1;
volatile int v2;

void sink (int, ...);

#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT(a, b)

#define T(str)						\
  __attribute__ ((noipa))				\
  void CAT (test_, __LINE__) (void) {			\
    int i0 = 0, i1 = i0 + 1, i2 = i1 + 1, i3 = i2 + 1;	\
    sink (strlen (str), i0, i1, i2, i3);		\
  } typedef void dummy_type

T (a);                /* { dg-warning "argument missing terminating nul" }  */
T (&a[0]);            /* { dg-warning "nul" }  */
T (&a[0] + 1);        /* { dg-warning "nul" }  */
T (&a[1]);            /* { dg-warning "nul" }  */
T (&a[v0]);           /* { dg-warning "nul" }  */
T (&a[v0] + 1);       /* { dg-warning "nul" }  */


const char b[][5] = { /* { dg-message "declared here" } */
  "12", "123", "1234", "54321"
};

T (b[0]);
T (b[1]);
T (b[2]);
T (b[3]);             /* { dg-warning "nul" }  */

T (b[i0]);
T (b[i1]);
T (b[i2]);
T (b[i3]);            /* { dg-warning "nul" }  */

T (b[v0]);

T (&b[i2][i1]);
T (&b[i2][i1] + i1);
T (&b[i2][v0]);
T (&b[i2][i1] + v0);

T (&b[2][1]);
T (&b[2][1] + i1);
T (&b[2][i0]);
T (&b[2][1] + i0);

T (&b[2][1]);
T (&b[2][1] + v0);
T (&b[2][v0]);

T (&b[3][1]);           /* { dg-warning "nul" }  */
T (&b[3][1] + 1);       /* { dg-warning "nul" }  */
T (&b[3][1] + i1);      /* { dg-warning "nul" }  */
T (&b[3][v0]);          /* { dg-warning "nul" }  */
T (&b[3][1] + v0);      /* { dg-warning "nul" }  */
T (&b[3][v0] + v1);     /* { dg-warning "nul" }  */

T (&b[i3][i1]);         /* { dg-warning "nul" }  */
T (&b[i3][i1] + 1);     /* { dg-warning "nul" }  */
T (&b[i3][i1] + i1);    /* { dg-warning "nul" }  */
T (&b[i3][v0]);         /* { dg-warning "nul" "pr86919" { xfail *-*-* } }  */
T (&b[i3][i1] + v0);    /* { dg-warning "nul" "pr86919" { xfail *-*-* } }  */
T (&b[i3][v0] + v1);    /* { dg-warning "nul" "pr86919" { xfail *-*-* } }  */

T (v0 ? "" : b[0]);
T (v0 ? "" : b[1]);
T (v0 ? "" : b[2]);
T (v0 ? "" : b[3]);               /* { dg-warning "nul" }  */
T (v0 ? b[0] : "");
T (v0 ? b[1] : "");
T (v0 ? b[2] : "");
T (v0 ? b[3] : "");               /* { dg-warning "nul" }  */

T (v0 ? "" : b[i0]);
T (v0 ? "" : b[i1]);
T (v0 ? "" : b[i2]);
/* The following is diagnosed but the warning location is wrong
   (the PRE pass loses it).  */
T (v0 ? "" : b[i3]);              /* { dg-warning "nul" }  */
T (v0 ? b[i0] : "");
T (v0 ? b[i1] : "");
T (v0 ? b[i2] : "");
T (v0 ? b[i3] : "");              /* { dg-warning "nul" }  */

T (v0 ? "1234" : b[3]);           /* { dg-warning "nul" }  */
T (v0 ? "1234" : b[i3]);          /* { dg-warning "nul" }  */
T (v0 ? b[3] : "1234");           /* { dg-warning "nul" }  */
T (v0 ? b[i3] : "1234");          /* { dg-warning "nul" }  */

T (v0 ? a : b[3]);                /* { dg-warning "nul" }  */
T (v0 ? b[0] : b[2]);
T (v0 ? b[2] : b[3]);             /* { dg-warning "nul" }  */
T (v0 ? b[3] : b[2]);             /* { dg-warning "nul" }  */

T (v0 ? a : b[i3]);               /* { dg-warning "nul" }  */
T (v0 ? b[i0] : b[i2]);
T (v0 ? b[i2] : b[i3]);           /* { dg-warning "nul" }  */
T (v0 ? b[i3] : b[i2]);           /* { dg-warning "nul" }  */

T (v0 ? b[0] : &b[3][0] + 1);     /* { dg-warning "nul" }  */
T (v0 ? b[0] : &b[3][0] + i1);    /* { dg-warning "nul" }  */
T (v0 ? b[1] : &b[3][1] + v0);    /* { dg-warning "nul" }  */

T (v0 ? b[i0] : &b[i3][i0] + i1);    /* { dg-warning "nul" }  */
T (v0 ? b[i0] : &b[i3][i0] + i1);    /* { dg-warning "nul" }  */
T (v0 ? b[i1] : &b[i3][i1] + v0);    /* { dg-warning "nul" }  */

T (v0 ? &b[3][1] + v0 : b[2]);    /* { dg-warning "nul" }  */
T (v0 ? &b[3][v0] : &b[3][v1]);   /* { dg-warning "nul" }  */


struct A { char a[5], b[5]; };

const struct A s = { "1234", "12345" };

T (s.a);
T (&s.a[0]);
T (&s.a[0] + 1);
T (&s.a[0] + v0);
T (&s.a[1]);
T (&s.a[1] + 1);
T (&s.a[1] + v0);

T (&s.a[i0]);
T (&s.a[i0] + i1);
T (&s.a[i0] + v0);
T (&s.a[i1]);
T (&s.a[i1] + i1);
T (&s.a[i1] + v0);

T (s.b);              /* { dg-warning "nul" }  */
T (&s.b[0]);          /* { dg-warning "nul" }  */
T (&s.b[0] + 1);      /* { dg-warning "nul" }  */
T (&s.b[0] + v0);     /* { dg-warning "nul" }  */
T (&s.b[1]);          /* { dg-warning "nul" }  */
T (&s.b[1] + 1);      /* { dg-warning "nul" }  */
T (&s.b[1] + i0);     /* { dg-warning "nul" }  */
T (&s.b[1] + v0);     /* { dg-warning "nul" }  */

T (&s.b[i0]);         /* { dg-warning "nul" }  */
T (&s.b[i0] + i1);    /* { dg-warning "nul" }  */
T (&s.b[i0] + v0);    /* { dg-warning "nul" "pr86919" { xfail *-*-* } }  */
T (&s.b[i1]);         /* { dg-warning "nul" }  */
T (&s.b[i1] + i1);    /* { dg-warning "nul" }  */
T (&s.b[i1] + v0);    /* { dg-warning "nul" "pr86919" { xfail *-*-* } }  */

struct B { struct A a[2]; };

const struct B ba[] = {
  { { { "123", "12345" }, { "12345", "123" } } },
  { { { "12345", "123" }, { "123", "12345" } } },
  { { { "1", "12" },      { "123", "1234" } } },
  { { { "123", "1234" },  { "12345", "12" } } }
};

T (ba[0].a[0].a);
T (&ba[0].a[0].a[0]);
T (&ba[0].a[0].a[0] + 1);
T (&ba[0].a[0].a[0] + v0);
T (&ba[0].a[0].a[1]);
T (&ba[0].a[0].a[1] + 1);
T (&ba[0].a[0].a[1] + v0);

T (ba[0].a[0].b);           /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[0]);       /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[0] + 1);   /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[0] + v0);  /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[1]);       /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[1] + 1);   /* { dg-warning "nul" }  */
T (&ba[0].a[0].b[1] + v0);  /* { dg-warning "nul" }  */

T (ba[0].a[1].a);           /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[0]);       /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[0] + 1);   /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[0] + v0);  /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[1]);       /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[1] + 1);   /* { dg-warning "nul" }  */
T (&ba[0].a[1].a[1] + v0);  /* { dg-warning "nul" }  */

T (ba[0].a[1].b);
T (&ba[0].a[1].b[0]);
T (&ba[0].a[1].b[0] + 1);
T (&ba[0].a[1].b[0] + v0);
T (&ba[0].a[1].b[1]);
T (&ba[0].a[1].b[1] + 1);
T (&ba[0].a[1].b[1] + v0);


T (ba[1].a[0].a);           /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[0]);       /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[0] + 1);   /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[0] + v0);  /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[1]);       /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[1] + 1);   /* { dg-warning "nul" }  */
T (&ba[1].a[0].a[1] + v0);  /* { dg-warning "nul" }  */

T (ba[1].a[0].b);
T (&ba[1].a[0].b[0]);
T (&ba[1].a[0].b[0] + 1);
T (&ba[1].a[0].b[0] + v0);
T (&ba[1].a[0].b[1]);
T (&ba[1].a[0].b[1] + 1);
T (&ba[1].a[0].b[1] + v0);

T (ba[1].a[1].a);
T (&ba[1].a[1].a[0]);
T (&ba[1].a[1].a[0] + 1);
T (&ba[1].a[1].a[0] + v0);
T (&ba[1].a[1].a[1]);
T (&ba[1].a[1].a[1] + 1);
T (&ba[1].a[1].a[1] + v0);

T (ba[1].a[1].b);           /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[0]);       /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[0] + 1);   /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[0] + v0);  /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[1]);       /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[1] + 1);   /* { dg-warning "nul" }  */
T (&ba[1].a[1].b[1] + v0);  /* { dg-warning "nul" }  */


T (ba[2].a[0].a);
T (&ba[2].a[0].a[0]);
T (&ba[2].a[0].a[0] + 1);
T (&ba[2].a[0].a[0] + v0);
T (&ba[2].a[0].a[1]);
T (&ba[2].a[0].a[1] + 1);
T (&ba[2].a[0].a[1] + v0);

T (ba[2].a[0].b);
T (&ba[2].a[0].b[0]);
T (&ba[2].a[0].b[0] + 1);
T (&ba[2].a[0].b[0] + v0);
T (&ba[2].a[0].b[1]);
T (&ba[2].a[0].b[1] + 1);
T (&ba[2].a[0].b[1] + v0);

T (ba[2].a[1].a);
T (&ba[2].a[1].a[0]);
T (&ba[2].a[1].a[0] + 1);
T (&ba[2].a[1].a[0] + v0);
T (&ba[2].a[1].a[1]);
T (&ba[2].a[1].a[1] + 1);
T (&ba[2].a[1].a[1] + v0);


T (ba[3].a[0].a);
T (&ba[3].a[0].a[0]);
T (&ba[3].a[0].a[0] + 1);
T (&ba[3].a[0].a[0] + v0);
T (&ba[3].a[0].a[1]);
T (&ba[3].a[0].a[1] + 1);
T (&ba[3].a[0].a[1] + v0);

T (ba[3].a[0].b);
T (&ba[3].a[0].b[0]);
T (&ba[3].a[0].b[0] + 1);
T (&ba[3].a[0].b[0] + v0);
T (&ba[3].a[0].b[1]);
T (&ba[3].a[0].b[1] + 1);
T (&ba[3].a[0].b[1] + v0);

T (ba[3].a[1].a);           /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[0]);	    /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[0] + 1);   /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[0] + v0);  /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[1]);	    /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[1] + 1);   /* { dg-warning "nul" }  */
T (&ba[3].a[1].a[1] + v0);  /* { dg-warning "nul" }  */

T (ba[3].a[1].b);
T (&ba[3].a[1].b[0]);
T (&ba[3].a[1].b[0] + 1);
T (&ba[3].a[1].b[0] + v0);
T (&ba[3].a[1].b[1]);
T (&ba[3].a[1].b[1] + 1);
T (&ba[3].a[1].b[1] + v0);


T (v0 ? ba[0].a[0].a : ba[0].a[0].b);           /* { dg-warning "nul" }  */
T (v0 ? ba[0].a[0].a : ba[0].a[0].b);           /* { dg-warning "nul" }  */

T (v0 ? &ba[0].a[0].a[0] : &ba[3].a[1].a[0]);   /* { dg-warning "nul" }  */
T (v0 ? &ba[3].a[1].a[1] :  ba[0].a[0].a);      /* { dg-warning "nul" }  */

T (v0 ? ba[0].a[0].a : ba[0].a[1].b);
T (v0 ? ba[0].a[1].b : ba[0].a[0].a);

T (v2 ? b[1] : &b[3][1] + v2);    /* { dg-warning "nul" }  */
T (v2 ? &b[3][1] + v2 : b[2]);    /* { dg-warning "nul" }  */
T (v2 ? &b[3][v2] : &b[2][v2]);   /* { dg-warning "nul" }  */
