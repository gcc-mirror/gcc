void *vp;
int (*ap)[];
struct S *sp;
union U *up;
int (*fp)();
 
void
test ()
{
    vp++;               /* ERROR - incrementing void * */
    ap++;               /* ERROR - incrementing ptr to incomplete type */
    sp++;               /* ERROR - incrementing ptr to incomplete type */
    up++;               /* ERROR - incrementing ptr to incomplete type */
    fp++;               /* ERROR - incrementing ptr to function */
}
