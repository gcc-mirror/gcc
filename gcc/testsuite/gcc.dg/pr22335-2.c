/* { dg-do compile } */
/* { dg-options "-O2"  } */
void abort(void) __attribute__((__noreturn__));
typedef struct {
  int container;
} edge_iterator;
struct ls_expr
{
  int index;
  int stores;
  struct ls_expr * next;
};
struct ls_expr * pre_ldst_mems;
edge_iterator ei;
void
store_motion (void)
{
  struct ls_expr * ptr, **prev_next_ptr_ptr;
  edge_iterator ei1;
  edge_iterator ei2;
  int a = ptr != ((void *)0);
  for (ptr = pre_ldst_mems, prev_next_ptr_ptr = &pre_ldst_mems;
       ptr != ((void *)0);
       ptr = *prev_next_ptr_ptr)
    if (!((ptr)->stores))
     *prev_next_ptr_ptr = ptr->next;
    else
      prev_next_ptr_ptr = &ptr->next;

  for (ptr = pre_ldst_mems; ptr != 0; ptr = ptr->next)
    ;
  ei1 = ei;
  ei2 = ei1;
  if (!ei2.container)
    abort ();
  ei2 = ei1;
  if (!ei2.container)
    abort ();
}
