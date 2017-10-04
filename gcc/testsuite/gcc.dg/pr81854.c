/* PR c/81854 - weak alias of an incompatible symbol accepted
   { dg-do compile }
   { dg-require-ifunc "" } */

const char* __attribute__ ((weak, alias ("f0_target")))
f0 (void);          /* { dg-error "alias between function and variable" } */

int f0_target;      /* { dg-message "aliased declaration here" } */


const char* __attribute__ ((weak, alias ("f1_target")))
f1 (void);          /* { dg-warning "alias between functions of incompatible types" } */

void f1_target (int *p)   /* { dg-message "aliased declaration here" } */
{
  *p = 0;
}


const char* __attribute__ ((alias ("f2_target")))
f2 (void*);   /* { dg-warning "alias between functions of incompatible types" } */

const char* f2_target (int i)   /* { dg-message "aliased declaration here" } */
{
  (void)&i;
  return 0;
}


int __attribute__ ((ifunc ("f3_resolver")))
f3 (void);          /* { dg-error ".ifunc. resolver must return a function pointer" } */

int f3_resolver (void)   /* { dg-message "resolver declaration here" } */
{
  return 0;
}


int __attribute__ ((ifunc ("f4_resolver")))
f4 (void);          /* { dg-warning ".ifunc. resolver should return a function pointer" } */

void* f4_resolver (void) /* { dg-message "resolver declaration here" } */
{
  return 0;
}


int __attribute__ ((ifunc ("f5_resolver")))
f5 (void);          /* { dg-warning "alias between functions of incompatible types" } */

typedef void F5 (void);
F5* f5_resolver (void) /* { dg-message "aliased declaration here" } */
{
  return 0;
}

const char* __attribute__ ((ifunc ("f6_resolver")))
f6 (void);

typedef const char* F6 (void);
F6* f6_resolver (void)
{
  return 0;
}
