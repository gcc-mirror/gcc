/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -pedantic-errors -maddress-space-conversion" } */

/* This is the same as errors3.c except it should not generate any errors.
   If you modify this code, please modify errors3.c as well.  */
typedef __ea int *ea_ptr_t;
typedef      int *lm_ptr_t;

ea_ptr_t ea, ea2;
lm_ptr_t lm, lm2;

extern void call_ea (ea_ptr_t);
extern void call_lm (lm_ptr_t);

/* No errors here.  */
void to_ea (void) { ea = lm; }
void to_lm (void) { lm = ea; }
ea_ptr_t ret_ea (void) { return lm; }
lm_ptr_t ret_lm (void) { return ea; }
void call_ea2 (void) { call_ea (lm); }
void call_lm2 (void) { call_lm (ea); }
int sub_ea (void) { return ea - lm; }
int sub_lm (void) { return lm - ea; }
int if_ea (int test) { return *((test) ? ea : lm); }
int if_lm (int test) { return *((test) ? lm : ea); }

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
