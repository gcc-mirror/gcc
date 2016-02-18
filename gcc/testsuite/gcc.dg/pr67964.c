/* PR c/67964 */
/* { dg-do compile } */
/* { dg-require-alias "" } */

extern int fn0 (void) __attribute__ ((const const)); /* { dg-error "expected" } */
extern int fn1 (void) __attribute__ ((const, const));
extern int fn2 (void) __attribute__ ((optimize (0) const)); /* { dg-error "expected" } */
extern int fn3 (void) __attribute__ ((optimize (0), const));
/* We allow starting/trailing comma.  */
extern int fn4 (void) __attribute__ ((, const));
extern int fn5 (void) __attribute__ ((const, ));
extern int fn6 (void) __attribute__ ((,,,, const,,,,, ));
extern int fn7 (void) __attribute__ ((,));
extern int fn8 (void) __attribute__ ((__noreturn__ __noreturn__)); /* { dg-error "expected" } */
extern int fn9 (void) __attribute__ ((__noreturn__, __noreturn__));
extern int fn10 (void) __attribute__ ((__cold__ __pure__ __noclone__)); /* { dg-error "expected" } */
extern int fn11 (void) __attribute__ ((__cold__, __pure__ __noclone__)); /* { dg-error "expected" } */
int i;
int ii;
extern int a __attribute__ ((alias ("i") unused)); /* { dg-error "expected" } */
extern int a2 __attribute__ ((alias ("i" "i")));
struct A { char p[6]; } __attribute__((__packed__ packed)); /* { dg-error "expected" } */
