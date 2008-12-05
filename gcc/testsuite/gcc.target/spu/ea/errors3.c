/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -pedantic-errors -mno-address-space-conversion" } */

/* This is the same as compile3.c except it should generate errors where
   pointers are mixed.  If you modify this code, please modify compile3.c as
   well.  */
typedef __ea int *ea_ptr_t;
typedef      int *lm_ptr_t;

ea_ptr_t ea, ea2;
lm_ptr_t lm, lm2;

extern void call_ea (ea_ptr_t);
extern void call_lm (lm_ptr_t);

/* Errors should be generated here.  */
void to_ea (void) { ea = lm; }				/* { dg-error "assignment to a pointer to an incompatible address space" } */
void to_lm (void) { lm = ea; }				/* { dg-error "assignment to a pointer to an incompatible address space" } */
ea_ptr_t ret_ea (void) { return lm; }			/* { dg-error "return of a pointer to an incompatible address space" } */
lm_ptr_t ret_lm (void) { return ea; }			/* { dg-error "return of a pointer to an incompatible address space" } */
void call_ea2 (void) { call_ea (lm); }			/* { dg-error "passing argument 1 of 'call_ea' is a pointer to an incompatible address space" } */
void call_lm2 (void) { call_lm (ea); }			/* { dg-error "passing argument 1 of 'call_lm' is a pointer to an incompatible address space" } */
int sub_ea (void) { return ea - lm; }			/* { dg-error "invalid operands to binary -" } */
int sub_lm (void) { return lm - ea; }			/* { dg-error "invalid operands to binary -" } */
int if_ea (int test) { return *((test) ? ea : lm); }	/* { dg-error "pointers to incompatible address spaces used in conditional expression" } */
int if_lm (int test) { return *((test) ? lm : ea); }	/* { dg-error "pointers to incompatible address spaces used in conditional expression" } */

/* No errors here.  */
void to_ea2 (void) { ea = ea2; }
void to_lm2 (void) { lm = lm2; }

void to_ea_with_cast (void) { ea = (ea_ptr_t)lm; }
void to_lm_with_cast (void) { lm = (lm_ptr_t)ea; }
ea_ptr_t ret_ea_with_cast (void) { return (ea_ptr_t)lm; }
lm_ptr_t ret_lm_with_cast (void) { return (lm_ptr_t)ea; }
void call_ea2_with_cast (void) { call_ea ((ea_ptr_t)lm); }
void call_lm2_with_cast (void) { call_lm ((lm_ptr_t)ea); }
int sub_ea_with_cast (void) { return ea - (ea_ptr_t)lm; }
int sub_lm_with_cast (void) { return lm - (lm_ptr_t)ea; }
int if_ea_with_cast (int test) { return *((test) ? ea : (ea_ptr_t)lm); }
int if_lm_with_cast (int test) { return *((test) ? lm : (lm_ptr_t)ea); }

void void_ea (void) { ea = (void *)0; }
void void_lm (void) { lm = (__ea void *)0; }
