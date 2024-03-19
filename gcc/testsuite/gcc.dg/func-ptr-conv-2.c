/* Conversions between function and object pointers are not permitted
   in any version of ISO C, even with casts, except for the special
   case of converting a null pointer constant to function pointer
   type.  Likewise, comparisons between function and object pointers
   are not permitted.  PR c/11234.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

void f(void);

void *v1 = f; /* { dg-warning "12:pointer" "bad conversion" } */
void *v2 = &f; /* { dg-warning "12:pointer" "bad conversion" } */
void *v3 = (void *)f; /* { dg-warning "12:pointer" "bad conversion" } */
void *v4 = (void *)&f; /* { dg-warning "12:pointer" "bad conversion" } */
void *v5;
char *c1 = f; /* { dg-error "12:pointer" "bad conversion" } */
char *c2 = &f; /* { dg-error "12:pointer" "bad conversion" } */
char *c3 = (char *)f; /* { dg-warning "12:pointer" "bad conversion" } */
char *c4 = (char *)&f; /* { dg-warning "12:pointer" "bad conversion" } */
char *c5;
void (*fp)(void);
int a;

void
g(void)
{
  v5 = f; /* { dg-warning "6:pointer" "bad conversion" } */
  v5 = &f; /* { dg-warning "6:pointer" "bad conversion" } */
  v5 = (void *)f; /* { dg-warning "8:pointer" "bad conversion" } */
  v5 = (void *)&f; /* { dg-warning "8:pointer" "bad conversion" } */
  c5 = f; /* { dg-error "6:pointer" "bad conversion" } */
  c5 = &f; /* { dg-error "6:pointer" "bad conversion" } */
  c5 = (char *)f; /* { dg-warning "8:pointer" "bad conversion" } */
  c5 = (char *)&f; /* { dg-warning "8:pointer" "bad conversion" } */
  fp = v5; /* { dg-warning "6:pointer" "bad conversion" } */
  fp = c5; /* { dg-error "6:pointer" "bad conversion" } */
  fp = (void (*)(void))v5; /* { dg-warning "8:pointer" "bad conversion" } */
  fp = (void (*)(void))c5; /* { dg-warning "8:pointer" "bad conversion" } */
  (a ? f : v3); /* { dg-warning "10:pointer" "bad conversion" } */
  (a ? v2 : fp); /* { dg-warning "11:pointer" "bad conversion" } */
  /* The following are OK.  */
  fp = 0;
  fp = (void *)0;
  fp = 0L;
  fp = (void (*)(void))0;
  fp = (void (*)(void))(void *)0;
  (a ? f : 0);
  (a ? f : (void *)0);
  (a ? (void *)0 : fp);
  (a ? 0 : fp);
}

/* The following are OK.  */
void (*fp2)(void) = 0;
void (*fp3)(void) = (void *)0;
