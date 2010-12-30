/* { dg-do compile } */

/* Test overload resolution of vector types.
   From Janis Johnson and Paolo Bonzini, based on PR/16882 */

#define vector __attribute__((vector_size(16)))

vector signed int vld (int a1, const vector signed int *a2) { return *a2; } /* { dg-message "vld|no known conversion" } */
vector signed short vld (int a1, const vector signed short *a2) { return *a2; } /* { dg-message "vld|no known conversion" } */

extern int i;
extern vector signed short vss;
extern vector signed char *vscp;
extern vector signed short *vssp;
extern const vector signed short *cvssp;

void foo ()
{
  vss = vld(i, vscp);        /* { dg-error "no matching function for call" } */
  // { dg-message "candidate" "candidate note" { target *-*-* } 19 }
  vss = vld(i, vssp);
  vss = vld(i, cvssp);
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */
