/* test that assignment drops qualifiers, Bug 98047 */
/* { dg-do compile } */
/* { dg-options "" } */


volatile int jv;
extern int j;
extern typeof(jv = 1) j;

_Atomic int ja;
extern typeof(ja = 1) j;

int * __restrict pa;
extern int *p;
extern typeof(pa = 0) p;



