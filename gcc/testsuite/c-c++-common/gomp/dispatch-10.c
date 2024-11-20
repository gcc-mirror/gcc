/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int *f();

struct t {
  int *a, *b;
};

void construct(int *x, int *y);
void noconstruct(int *x, int *y);

#pragma omp declare variant(construct) match(construct={dispatch}) adjust_args(need_device_ptr: x,y)
#pragma omp declare variant(noconstruct) match(implementation={vendor(gnu)})
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

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_default_device" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device" 4 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(0\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\\.\[0-9\]+\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = s->b;\[\r\n\]* *D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(D\\.\[0-9\]+, 0\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = f \\(\\);\[\r\n\]* *D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(D\\.\[0-9\]+, 0\\);" 1 "gimple" } } */
