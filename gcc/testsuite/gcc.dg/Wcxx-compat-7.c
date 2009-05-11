/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

struct s1
{
  enum e1	/* { dg-message "note: enum type defined here" } */
  {
    A,		/* { dg-message "note: enum constant defined here" } */
    B
  } f1;
  struct s2	/* { dg-message "note: struct defined here" } */
  {
    struct s3	/* { dg-message "note: struct defined here" } */
    {
      enum e1 f3;
      struct s1 *p1;
      struct s2 *p2;
      struct s3 *p3;
    } f2;
    union u1	/* { dg-message "note: union defined here" } */
    {
      int f4;
    } f5;
    struct s3 f6;
  } f7;
  struct s2 f8;
  enum e1 f9;
};

struct s1 v1;
enum e1 v2;	/* { dg-warning "not visible in C\[+\]\[+\]" } */
struct s2 v3;	/* { dg-warning "not visible in C\[+\]\[+\]" } */
struct s3 v4;	/* { dg-warning "not visible in C\[+\]\[+\]" } */
union u1 v5;	/* { dg-warning "not visible in C\[+\]\[+\]" } */
int i = A;	/* { dg-warning "not visible in C\[+\]\[+\]" } */
