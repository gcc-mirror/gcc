/* { dg-do compile } */

/* Check that the missing declaration for construct does not trigger an ICE but 
   is rejected as invalid code.  */

int *f();

struct t {
  int *a, *b;
};

#pragma omp declare variant(construct) match(construct={dispatch}) adjust_args(need_device_ptr: x,y) /* { dg-error "'construct' undeclared here \\(not in a function\\)" } */
#pragma omp declare variant(noconstruct) match(implementation={vendor(gnu)}) /* { dg-error "'noconstruct' undeclared here \\(not in a function\\)" } */
void bar(int *x, int *y);

int nocontext, novariant;

void sub(struct t *s, void *y)
{
    bar( f(), s->b);
 #pragma omp dispatch device(0) is_device_ptr(s)
    bar( f(), s->b);
    
    bar( (int *) y, s->b);
 #pragma omp dispatch device(0) is_device_ptr(y)
    bar( (int *) y, s->b);
}



