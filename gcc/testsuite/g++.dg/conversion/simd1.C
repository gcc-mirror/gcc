/* { dg-do compile } */

/* Test overload resolution of vector types.
   From Janis Johnson and Paolo Bonzini, based on PR/16882 */

#define vector __attribute__((vector_size(16)))

vector signed int vld (int a1, const vector signed int *a2) { return *a2; } /* { dg-error "near match" } */
/* { dg-warning "vector returned by ref" "" { target { powerpc*-*-linux* && ilp32 } }  8 } */
vector signed short vld (int a1, const vector signed short *a2) { return *a2; } /* { dg-error "near match" } */

extern int i;
extern vector signed short vss;
extern vector signed char *vscp;
extern vector signed short *vssp;
extern const vector signed short *cvssp;

void foo ()
{
  vss = vld(i, vscp);        /* { dg-error "no match" } */
  vss = vld(i, vssp);
  vss = vld(i, cvssp);
}
