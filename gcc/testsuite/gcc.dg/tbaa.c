/* { dg-do assemble } */
/* { dg-options "-O2 -flto -flto-partition=one -fdump-tree-evrp -std=gnu89" } */
/* { dg-require-effective-target lto } */

typedef struct rtx_def *rtx;
typedef struct cselib_val_struct
{
  union
  {
  } u;
  struct elt_loc_list *locs;
}
cselib_val;
struct elt_loc_list
{
  struct elt_loc_list *next;
  rtx loc;
};
static int n_useless_values;
unchain_one_elt_loc_list (pl)
     struct elt_loc_list **pl;
{
  struct elt_loc_list *l = *pl;
  *pl = l->next;
}

discard_useless_locs (x, info)
     void **x;
{
  cselib_val *v = (cselib_val *) * x;
  struct elt_loc_list **p = &v->locs;
  int had_locs = v->locs != 0;
  while (*p)
    {
      unchain_one_elt_loc_list (p);
      p = &(*p)->next;
    }
  if (had_locs && v->locs == 0)
    {
      n_useless_values++;
    }
}
/* { dg-final { scan-tree-dump-times "n_useless_values" 2 "evrp" { xfail *-*-* } } } */
