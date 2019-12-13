// { dg-do assemble  }
void *vp;
int (*ap)[];
struct S *sp;
union U *up;
int (*fp)();
 
void
test ()
{
    vp++;               /* { dg-error "5:ISO C\\+\\+ forbids incrementing" } incrementing void * */
    ap++;               /* { dg-error "5:cannot increment" } incrementing ptr to incomplete type */
    sp++;               /* { dg-error "5:cannot increment" } incrementing ptr to incomplete type */
    up++;               /* { dg-error "5:cannot increment" } incrementing ptr to incomplete type */
    fp++;               /* { dg-error "5:ISO C\\+\\+ forbids incrementing" } incrementing ptr to function */
}
