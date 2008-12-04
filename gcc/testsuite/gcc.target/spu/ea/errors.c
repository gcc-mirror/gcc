/* Invalid __ea declarations.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

extern __ea void f1 ();	 /* { dg-error "'__ea' specified for function 'f1'" } */

void func ()
{
  register __ea int local1; /* { dg-error "'__ea' combined with 'register' qualifier for 'local1'" } */
  auto __ea int local2;     /* { dg-error "'__ea' combined with 'auto' qualifier for 'local2'" } */
  __ea int local3;	    /* { dg-error "'__ea' specified for auto variable 'local3'" } */
  register int *__ea p1;    /* { dg-error "'__ea' combined with 'register' qualifier for 'p1'" } */
  auto char *__ea p2;       /* { dg-error "'__ea' combined with 'auto' qualifier for 'p2'" } */
  void *__ea p3;            /* { dg-error "'__ea' specified for auto variable 'p3'" } */
  register __ea int a1[2];  /* { dg-error "'__ea' combined with 'register' qualifier for 'a1'" } */
  auto __ea char a2[1];     /* { dg-error "'__ea' combined with 'auto' qualifier for 'a2'" } */
  __ea char a3[5];          /* { dg-error "'__ea' specified for auto variable 'a3'" } */
}

void func2 (__ea int x)	    /* { dg-error "'__ea' specified for parameter 'x'" } */
{ }

struct st {
  __ea int x;		    /* { dg-error "'__ea' specified for structure field 'x'" } */
  int *__ea q;		    /* { dg-error "'__ea' specified for structure field 'q'" } */
} s;

__ea int func3 (int x) {    /* { dg-error "'__ea' specified for function 'func3'" } */
  return x;
}

struct A { int a; };

int func4 (int *__ea x)	    /* { dg-error "'__ea' specified for parameter 'x'" } */
{
  struct A i = (__ea struct A) { 1 };	/* { dg-error "compound literal qualified by address-space qualifier" } */
  return i.a;
}

extern __ea int ea_var;		/* { dg-error "previous.*decl" } */
int ea_var;			/* { dg-error "conflicting named address spaces" } */
