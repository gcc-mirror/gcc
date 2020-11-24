/* test that lvalue conversions drops qualifiers, Bug 97702 */
/* { dg-do compile } */
/* { dg-options "" } */


const int jc;
extern int j;
extern typeof(0,jc) j;
extern typeof(+jc) j;
extern typeof(-jc) j;
extern typeof(1?jc:0) j;
extern typeof((int)jc) j;
extern typeof((const int)jc) j;

volatile int kv;
extern int k;
extern typeof(0,kv) k;
extern typeof(+kv) k;
extern typeof(-kv) k;
extern typeof(1?kv:0) k;
extern typeof((int)kv) k;
extern typeof((volatile int)kv) k;

_Atomic int la;
extern int l;
extern typeof(0,la) l;
extern typeof(+la) l;
extern typeof(-la) l;
extern typeof(1?la:0) l;
extern typeof((int)la) l;
extern typeof((_Atomic int)la) l;

int * restrict mr;
extern int *m;
extern typeof(0,mr) m;
extern typeof(1?mr:0) m;
extern typeof((int *)mr) m;
extern typeof((int * restrict)mr) m;


