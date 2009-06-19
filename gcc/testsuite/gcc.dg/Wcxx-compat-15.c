/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

typedef int myint1;
typedef int myint2;
typedef int myint3;
struct s1
{
  myint1 myint1;	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  myint2 *myint2;	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  int myint3;
  struct s2
  {
    myint3 f2;		/* { dg-warning "C\[+\]\[+\]" } */
  } f1;
};

struct s3
{
  int myint1;
  struct s4
  {
    int myint1;
  } f1;
  struct s5
  {
    int myint1;
    struct s6
    {
      myint1 f4;	/* { dg-warning "C\[+\]\[+\]" } */
    } f3;
  } f2;
};
