/* PR tree-optimization/102238 - missing -Wrestrict on sprintf formatting
   a struct member into enclosing object
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-format-overflow" } */

extern int sprintf (char*, const char*, ...);

extern void sink (void*, ...);

struct A
{
  char a[4];
};

struct B
{
  struct A a1, a2;
};

extern struct B eb;

enum { B_a2_a_off = __builtin_offsetof (struct B, a2.a) };


void test_warn_src_decl_plus (void)
{
  {
    char *s = (char*)&eb + B_a2_a_off;
    char *d = eb.a2.a;
    sprintf (d, "%s", s);     // { dg-warning "overlaps" }
  }

  {
    // If strlen (s) > 0 there is overlap with a[1].
    char *s = (char*)&eb + B_a2_a_off + 1;
    char *d = eb.a2.a;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // strlen (s) must be at most 1 so there can be no overlap with a.
    char *s = (char*)&eb + B_a2_a_off + 2;
    char *d = eb.a2.a;
    sprintf (d, "%s", s);     // { dg-bogus "-Wrestrict" }
  }

  {
    // strlen (s) must be at most 0 so there can be no overlap with a.
    char *s = (char*)&eb + B_a2_a_off + 3;
    char *d = eb.a2.a;
    sprintf (d, "%s", s);     // { dg-bogus "-Wrestrict" }
  }
}


void test_warn_src_ptr_plus (struct B *p)
{
  {
    char *s = (char*)p + B_a2_a_off;
    char *d = p->a2.a;
    sprintf (d, "%s", s);     // { dg-warning "overlaps" }
  }

  {
    // If strlen (s) > 0 there is overlap with a[1].
    char *s = (char*)p + B_a2_a_off + 1;
    char *d = p->a2.a;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // strlen (s) must be at most 1 so there can be no overlap with a.
    char *s = (char*)p + B_a2_a_off + 2;
    char *d = p->a2.a;
    sprintf (d, "%s", s);     // { dg-bogus "-Wrestrict" }
  }

  {
    // strlen (s) must be at most 0 so there can be no overlap with a.
    char *s = (char*)p + B_a2_a_off + 3;
    char *d = p->a2.a;
    sprintf (d, "%s", s);     // { dg-bogus "-Wrestrict" }
  }
}


void test_warn_dst_decl_plus (void)
{
  {
    char *s = eb.a2.a;
    char *d = (char*)&eb + B_a2_a_off;
    sprintf (d, "%s", s);     // { dg-warning "overlaps" }
  }

  {
    // If strlen (a) > 0 there is overlap with a[1].
    char *s = eb.a2.a;
    char *d = (char*)&eb + B_a2_a_off + 1;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // If strlen (a) > 1 there is overlap with a[2].
    char *s = eb.a2.a;
    char *d = (char*)&eb + B_a2_a_off + 2;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // If strlen (a) > 2 there is overlap with a[3].
    char *s = eb.a2.a;
    char *d = (char*)&eb + B_a2_a_off + 3;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }
}


void test_warn_dst_ptr_plus (struct B *p)
{
  {
    char *s = p->a2.a;
    char *d = (char*)p + B_a2_a_off;
    sprintf (d, "%s", s);     // { dg-warning "overlaps" }
  }

  {
    // If strlen (a) > 0 there is overlap with a[1].
    char *s = p->a2.a;
    char *d = (char*)p + B_a2_a_off + 1;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // If strlen (a) > 1 there is overlap with a[2].
    char *s = p->a2.a;
    char *d = (char*)p + B_a2_a_off + 2;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }

  {
    // If strlen (a) > 2 there is overlap with a[3].
    char *s = p->a2.a;
    char *d = (char*)p + B_a2_a_off + 3;
    sprintf (d, "%s", s);     // { dg-warning "may overlap" }
  }
}
