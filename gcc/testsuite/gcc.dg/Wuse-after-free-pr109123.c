/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;
extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__warn_unused_result__)) __attribute__ ((__alloc_size__ (2)));
struct vector_objective; 
typedef struct vector_objective vector_objective;
struct vector_objective { double *_begin; double *_end; double *_capacity; };
static inline size_t vector_objective_size(const vector_objective * v) { 
    return v->_end - v->_begin;  /* { dg-bogus "used after" } */
}
static inline size_t vector_objective_capacity(const vector_objective * v) {
    return v->_capacity - v->_begin;
}
static inline void vector_objective_reserve(vector_objective * v, size_t n) {
    size_t old_capacity = vector_objective_capacity(v);
    size_t old_size = vector_objective_size(v);
    if (n > old_capacity) {
        v->_begin = realloc(v->_begin, sizeof(double) * n);
        v->_end = v->_begin + old_size;
        v->_capacity = v->_begin + n;
    }
}
static inline void vector_objective_push_back(vector_objective * v, double x) {
    if (v->_end == v->_capacity)
        vector_objective_reserve (v, (vector_objective_capacity (v) == 0) ? 8 : 2 * vector_objective_capacity (v));
    *(v->_end) = x;
    v->_end++;
}

typedef struct {
    vector_objective xy;
} eaf_polygon_t;

int
rectangle_add(eaf_polygon_t * regions, double lx)
{
  vector_objective_push_back(&regions->xy, lx);
  return 0;
}
