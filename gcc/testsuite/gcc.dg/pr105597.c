/* PR tree-optimization/105597 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-int-conversion" } */

typedef struct {
  int allocated;
} vvec;

int vvneeds_want, mgpssort;

void vvinit(vvec *v, int minelems) { v->allocated = -minelems; }

void vvneeds(vvec *v, int needed) {
  if (needed > v->allocated)
    if (v->allocated < 0)
      ;
    else {
      int next = v->allocated + (v->allocated >> 1);
      vvneeds_want = next;
    }
}

void mgpssort_1() {
  vvinit((vvec *) &mgpssort, mgpssort_1);
  vvneeds((vvec *) &mgpssort, mgpssort_1);
}

