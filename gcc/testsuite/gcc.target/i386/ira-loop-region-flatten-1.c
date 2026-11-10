/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -mapxf -fdump-rtl-ira" } */

/* The pseudos of the outer walk live through the two list walks without
   a reference, so the list walks stay regions and the list pointer gets
   a call-saved register.  */

typedef double fp_t;
typedef struct { void (*process)(const void *, void *); } vtable;
typedef struct { unsigned int id; void *next; vtable *vt; } item;
typedef struct item_list { struct item_list *next; item *obj; } item_list;

typedef struct {
  int dim_x, dim_y;
  fp_t origin_x, origin_y, step_x, step_y;
  item_list **buckets;
} spatial_grid;

typedef struct {
  fp_t pos_x, pos_y, dir_x, dir_y, limit;
  unsigned int flags;
  unsigned long seq;
  unsigned long *seen;
} query;

extern int check_bounds(const spatial_grid *, const query *, fp_t *, fp_t *);

void traverse_grid(const spatial_grid *grid, query *q) {
  fp_t tnear, tfar, tmax_x, tmax_y, tdelta_x, tdelta_y, cx, cy;
  int cur_x, cur_y, inc_x, inc_y, end_x, end_y;
  long idx, stride_y;
  unsigned long seq = q->seq;
  unsigned long *seen = q->seen;
  item_list *cur;

  if (!check_bounds(grid, q, &tnear, &tfar))
    return;

  cx = q->pos_x + q->dir_x * tnear;
  cy = q->pos_y + q->dir_y * tnear;
  cur_x = (int)((cx - grid->origin_x) / grid->step_x);
  cur_y = (int)((cy - grid->origin_y) / grid->step_y);

  tmax_x = tnear + ((cur_x * grid->step_x + grid->origin_x) - cx) / q->dir_x;
  tdelta_x = grid->step_x / q->dir_x;
  inc_x = (q->dir_x > 0) ? 1 : -1;
  end_x = (q->dir_x > 0) ? grid->dim_x : -1;

  tmax_y = tnear + ((cur_y * grid->step_y + grid->origin_y) - cy) / q->dir_y;
  tdelta_y = grid->step_y / q->dir_y;
  inc_y = (q->dir_y > 0) ? 1 : -1;
  end_y = (q->dir_y > 0) ? grid->dim_y : -1;

  stride_y = inc_y * (long)grid->dim_x;
  idx = cur_y * (long)grid->dim_x + cur_x;

  cur = grid->buckets[idx];
  while (cur != 0) {
    if (seen[cur->obj->id] != seq) {
      seen[cur->obj->id] = seq;
      cur->obj->vt->process(cur->obj, q);
    }
    cur = cur->next;
  }

  for (;;) {
    if (tmax_x < tmax_y) {
      cur_x += inc_x;
      if (q->limit < tmax_x || cur_x == end_x) break;
      tmax_x += tdelta_x;
      idx += inc_x;
    } else {
      cur_y += inc_y;
      if (q->limit < tmax_y || cur_y == end_y) break;
      tmax_y += tdelta_y;
      idx += stride_y;
    }

    cur = grid->buckets[idx];
    while (cur != 0) {
      if (seen[cur->obj->id] != seq) {
        seen[cur->obj->id] = seq;
        cur->obj->vt->process(cur->obj, q);
      }
      cur = cur->next;
    }
  }
}

/* No caller-saved spills around indirect calls in the loop.  */

/* { dg-final { scan-rtl-dump {Loop 1 flatten-cost:[^\n]*keep} "ira" } } */
/* { dg-final { scan-rtl-dump {Loop 3 flatten-cost:[^\n]*keep} "ira" } } */
/* { dg-final { scan-assembler-not {call\t\*[^\n]*\n\tmovq\t[0-9]+\(%rsp\), %r} } } */
