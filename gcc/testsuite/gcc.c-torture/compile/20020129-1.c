/* Test call to static variable.  */

/* { dg-require-effective-target indirect_calls } */

typedef struct
{
  long long a[10];
} A;
  
void bar (A *);
  
typedef int (*B)(int);
  
void foo (void)
{
  static A a;
  bar (&a);
  (*(B)&a) (1);
}
