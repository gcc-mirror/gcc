// { dg-do assemble  }
void *vp;
int (*ap)[];
struct S *sp;
union U *up;
int (*fp)();
 
void
test ()
{
    vp++;               /* { dg-error "" } incrementing void * */
    ap++;               /* { dg-error "" } incrementing ptr to incomplete type */
    sp++;               /* { dg-error "" } incrementing ptr to incomplete type */
    up++;               /* { dg-error "" } incrementing ptr to incomplete type */
    fp++;               /* { dg-error "" } incrementing ptr to function */
}
