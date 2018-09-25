/* PR tree-optimization/86552 - missing warning for reading past the end
   of non-string arrays
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

extern char* stpcpy (char*, const char*);

const char a[5] = "12345";   /* { dg-message "declared here" } */

int v0 = 0;
int v1 = 1;
int v2 = 1;
int v3 = 1;

void sink (char*, ...);

#define T(str) sink (stpcpy (d, str))

void test_one_dim_array (char *d)
{
  T (a);                /* { dg-warning "argument missing terminating nul" }  */
  T (&a[0]);            /* { dg-warning "nul" }  */
  T (&a[0] + 1);        /* { dg-warning "nul" }  */
  T (&a[1]);            /* { dg-warning "nul" }  */

  int i0 = 0;
  int i1 = i0 + 1;

  T (&a[i0]);           /* { dg-warning "nul" }  */
  T (&a[i0] + 1);       /* { dg-warning "nul" }  */
  T (&a[i1]);           /* { dg-warning "nul" }  */

  T (&a[v0]);           /* { dg-warning "nul" }  */
  T (&a[v0] + 1);       /* { dg-warning "nul" }  */
  T (&a[v0] + v1);      /* { dg-warning "nul" }  */
}

const char b[][5] = { /* { dg-message "declared here" } */
  "12", "123", "1234", "54321"
};

void test_two_dim_array (char *d)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;

  T (b[0]);
  T (b[1]);
  T (b[2]);
  T (b[3]);             /* { dg-warning "nul" }  */
  T (b[i0]);
  T (b[i1]);
  T (b[i2]);
  T (b[i3]);            /* { dg-warning "nul" }  */
  T (b[v0]);
  T (b[v3]);

  T (&b[2][1]);
  T (&b[2][1] + 1);
  T (&b[2][v0]);
  T (&b[2][1] + v0);

  T (&b[i2][i1]);
  T (&b[i2][i1] + i1);
  T (&b[i2][v0]);
  T (&b[i2][i1] + v0);

  T (&b[3][1]);         /* { dg-warning "nul" }  */
  T (&b[3][1] + 1);     /* { dg-warning "nul" }  */
  T (&b[3][v0]);        /* { dg-warning "nul" }  */
  T (&b[3][1] + v0);    /* { dg-warning "nul" }  */
  T (&b[3][v0] + v1);   /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (&b[i3][i1]);       /* { dg-warning "nul" }  */
  T (&b[i3][i1] + i1);  /* { dg-warning "nul" }  */
  T (&b[i3][v0]);       /* { dg-warning "nul" }  */
  T (&b[i3][i1] + v0);  /* { dg-warning "nul" }  */
  T (&b[i3][v0] + v1);  /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? "" : b[0]);
  T (v0 ? "" : b[1]);
  T (v0 ? "" : b[2]);
  T (v0 ? "" : b[3]);               /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? b[0] : "");
  T (v0 ? b[1] : "");
  T (v0 ? b[2] : "");
  T (v0 ? b[3] : "");               /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? "1234" : b[3]);           /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? b[3] : "1234");           /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? a : b[3]);                /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? b[0] : b[2]);
  T (v0 ? b[2] : b[3]);             /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? b[3] : b[2]);             /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? b[0] : &b[3][0] + 1);     /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? b[1] : &b[3][1] + v0);    /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  /* It's possible to detect the missing nul in the following two
     expressions but GCC doesn't do it yet.  */
  T (v0 ? &b[3][1] + v0 : b[2]);    /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? &b[3][v0] : &b[3][v1]);   /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
}

struct A { char a[5], b[5]; };

const struct A s = { "1234", "12345" };

void test_struct_member (char *d)
{
  int i0 = 0;
  int i1 = i0 + 1;

  T (s.a);
  T (&s.a[0]);
  T (&s.a[0] + 1);
  T (&s.a[0] + i0);
  T (&s.a[1]);
  T (&s.a[1] + 1);
  T (&s.a[1] + i0);

  T (&s.a[i0]);
  T (&s.a[i0] + 1);
  T (&s.a[i0] + v0);
  T (&s.a[i1]);
  T (&s.a[i1] + 1);
  T (&s.a[i1] + v0);

  T (s.a);
  T (&s.a[0]);
  T (&s.a[0] + 1);
  T (&s.a[0] + v0);
  T (&s.a[1]);
  T (&s.a[1] + 1);
  T (&s.a[1] + v0);

  T (&s.a[i0]);
  T (&s.a[i0] + 1);
  T (&s.a[i0] + v0);
  T (&s.a[i1]);
  T (&s.a[i1] + 1);
  T (&s.a[i1] + v0);

  T (&s.a[v0]);
  T (&s.a[v0] + 1);
  T (&s.a[v0] + v0);
  T (&s.a[v1]);
  T (&s.a[v1] + 1);
  T (&s.a[v1] + v0);

  T (s.b);              /* { dg-warning "nul" }  */
  T (&s.b[0]);          /* { dg-warning "nul" }  */
  T (&s.b[0] + 1);      /* { dg-warning "nul" }  */
  T (&s.b[0] + i0);     /* { dg-warning "nul" }  */
  T (&s.b[1]);          /* { dg-warning "nul" }  */
  T (&s.b[1] + 1);      /* { dg-warning "nul" }  */
  T (&s.b[1] + i0);     /* { dg-warning "nul" }  */

  T (s.b);              /* { dg-warning "nul" }  */
  T (&s.b[0]);          /* { dg-warning "nul" }  */
  T (&s.b[0] + 1);      /* { dg-warning "nul" }  */
  T (&s.b[0] + v0);     /* { dg-warning "nul" }  */
  T (&s.b[1]);          /* { dg-warning "nul" }  */
  T (&s.b[1] + 1);      /* { dg-warning "nul" }  */
  T (&s.b[1] + v0);     /* { dg-warning "nul" }  */

  T (s.b);              /* { dg-warning "nul" }  */
  T (&s.b[v0]);         /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (&s.b[v0] + 1);     /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (&s.b[v0] + v0);    /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (&s.b[v1]);         /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (&s.b[v1] + 1);     /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (&s.b[v1] + v0);    /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
}

struct B { struct A a[2]; };

const struct B ba[] = {
  { { { "123", "12345" }, { "12345", "123" } } },
  { { { "12345", "123" }, { "123", "12345" } } },
  { { { "1", "12" },      { "123", "1234" } } },
  { { { "123", "1234" },  { "12345", "12" } } }
};

void test_array_of_structs (char *d)
{
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
  T (&ba[3].a[1].a[0]);	      /* { dg-warning "nul" }  */
  T (&ba[3].a[1].a[0] + 1);   /* { dg-warning "nul" }  */
  T (&ba[3].a[1].a[0] + v0);  /* { dg-warning "nul" }  */
  T (&ba[3].a[1].a[1]);	      /* { dg-warning "nul" }  */
  T (&ba[3].a[1].a[1] + 1);   /* { dg-warning "nul" }  */
  T (&ba[3].a[1].a[1] + v0);  /* { dg-warning "nul" }  */

  T (ba[3].a[1].b);
  T (&ba[3].a[1].b[0]);	
  T (&ba[3].a[1].b[0] + 1);
  T (&ba[3].a[1].b[0] + v0);
  T (&ba[3].a[1].b[1]);	
  T (&ba[3].a[1].b[1] + 1);
  T (&ba[3].a[1].b[1] + v0);


  T (v0 ? ba[0].a[0].a : ba[0].a[0].b);           /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? ba[0].a[0].a : ba[0].a[0].b);           /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? &ba[0].a[0].a[0] : &ba[3].a[1].a[0]);   /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */
  T (v0 ? &ba[3].a[1].a[1] :  ba[0].a[0].a);      /* { dg-warning "nul" "bug ???" { xfail *-*-* } }  */

  T (v0 ? ba[0].a[0].a : ba[0].a[1].b);
  T (v0 ? ba[0].a[1].b : ba[0].a[0].a);
}

/* { dg-prune-output " reading \[1-9\]\[0-9\]? bytes from a region " } */
