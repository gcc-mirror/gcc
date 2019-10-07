/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

typedef struct A { int i; } B;
typedef struct T { int i; } *T; /* { dg-warning "using 'T' as both a typedef and a tag is invalid" } */
typedef const float TFA;
typedef TFA TFB;
typedef TFB TFC;
typedef int IA[];
typedef IA *IAP;
extern IAP arr[];

void fn1 (B *); /* { dg-message "expected 'B \\*' {aka 'struct A \\*'} but argument is of type 'struct B \\*'" } */
void fn2 (TFC *);

void 
bar (B *b, int *i)
{
  fn1 ((struct B *) b); /* { dg-warning "passing argument" } */
  fn2 (i); /* { dg-warning "passing argument" } */
  sizeof (arr); /* { dg-error "invalid application of .sizeof. to incomplete type .int \\(\\*\\\[\\\]\\)\\\[\\\]." } */
}

int
foo (void *a)
{
  T t = a; /* { dg-warning "request for implicit conversion from 'void \\*' to 'T' {aka 'struct T \\*'} not" } */
  return t->i;
}
