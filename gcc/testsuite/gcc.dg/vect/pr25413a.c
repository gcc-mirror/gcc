/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

#define N 8

typedef __SIZE_TYPE__ size_t;

extern void *malloc (size_t __size) __attribute__ ((__nothrow__,
						    __malloc__));

typedef double num_t;
static const num_t num__infty = ((num_t)1.0)/((num_t)0.0);

struct oct_tt;
typedef struct oct_tt oct_t;

typedef unsigned int var_t;
typedef enum {
  OCT_EMPTY = 0,
  OCT_NORMAL = 1,
  OCT_CLOSED = 2
} oct_state;

struct oct_tt {
  var_t n;

  int ref;

  oct_state state;
  struct oct_tt* closed;

  num_t* c;
};

void* octfapg_mm_malloc (size_t t);
oct_t* octfapg_alloc (var_t n);
oct_t* octfapg_full_copy (oct_t* m);

struct mmalloc_tt;
typedef struct mmalloc_tt mmalloc_t;

struct mmalloc_tt
{
  int id;

  int nb_alloc;
  int nb_realloc;
  int nb_free;

  size_t rem;
  size_t max;
  size_t tot;

};

typedef struct
{
  size_t size;

  mmalloc_t* mm;
  int id;

  double dummy;

} mmheader_t;

void*
octfapg_mm_malloc (size_t t)
{
  char* m = (char*)malloc(t+sizeof(mmheader_t));
  return m+sizeof(mmheader_t);
}

oct_t* octfapg_empty (var_t n);

oct_t*
octfapg_empty (const var_t n)
{
  oct_t* m;
  /*octfapg_timing_enter("oct_empty",3);*/
  m = ((oct_t*) octfapg_mm_malloc (sizeof(oct_t)));
  m->n = n;
  m->ref = 1;
  m->state = OCT_EMPTY;
  m->closed = (oct_t*)((void *)0);
  m->c = (num_t*)((void *)0);
  /*octfapg_timing_exit("oct_empty",3);*/
  return m;
}

oct_t*
octfapg_alloc (const var_t n)
{
  size_t nn = (2*(size_t)(n)*((size_t)(n)+1));
  oct_t* m;
  m = octfapg_empty(n);
  m->c = ((num_t*) octfapg_mm_malloc (sizeof(num_t)*(nn)));
  ;
  m->state = OCT_NORMAL;
  m->closed = (oct_t*)((void *)0);
  return m;
}

oct_t*
octfapg_universe (const var_t n)
{
  oct_t* m;
  size_t i, nn = (2*(size_t)(n)*((size_t)(n)+1));
  m = octfapg_alloc(n);
  for (i=0;i<nn;i++) *(m->c+i) = num__infty;
  for (i=0;i<2*n;i++) *(m->c+((size_t)(i)+(((size_t)(i)+1)*((size_t)(i)+1))/2)) = (num_t)(0);
  m->state = OCT_CLOSED;
  return m;
}

int main (void)
{ 
  int i;
  check_vect ();
  
  oct_t *p = octfapg_universe(10);
  return 0;
} 

/* The second loop in octfapg_universe requires long multiply to do the vectorization. */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! vect_long_mult } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target vect_long_mult  } } } */
/* { dg-final { scan-tree-dump-times "vector alignment may not be reachable" 1 "vect" { target { ! vector_alignment_reachable  } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { ! vector_alignment_reachable } } } } */
