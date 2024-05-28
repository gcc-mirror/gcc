 /* Testing the correct usage of attribute counted_by in c23, multiple
  * definitions of the same tag in same or different scopes.
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

/* Allowed redefinitions of the same struct in the same scope, with the
   same counted_by attribute.  */
struct f {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (b))); };
struct f {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (b))); };
struct f {
  int b;
  int c;
  int a[]; }; /* { dg-error "redefinition of struct or union" } */

/* Error when the counted_by attribute is defined differently.  */
struct f {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (c))); }; /* { dg-error "redefinition of struct or union" } */

struct h {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (b))); } p;  

void test (void)
{
  struct h {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (b))); } x;

  p = x;
}

void test1 (void)
{
  struct h {
  int b;
  int c;
  int a[] __attribute__ ((counted_by (c))); } y;

  p = y;   /* { dg-error "incompatible types when assigning to type" } */
}

struct nested_f {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (b)));
}; 

struct nested_f {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (b)));
}; 

struct nested_f {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (n)));
};  /* { dg-error "redefinition of struct or union" } */

struct nested_h {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (b)));
} nested_p; 

void test_2 (void)
{
struct nested_h {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (b)));
} nested_x; 

 nested_p = nested_x;
}

void test_3 (void)
{
struct nested_h {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  char c[] __attribute__ ((counted_by (n)));
} nested_y; 

 nested_p = nested_y; /* { dg-error "incompatible types when assigning to type" } */
}
