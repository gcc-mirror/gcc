typedef struct gfc_formal_arglist
{
  struct gfc_formal_arglist* next;
}
gfc_actual_arglist;
struct gfc_formal_arglist*
update_arglist_pass (gfc_actual_arglist* lst, int po, unsigned argpos,
       const char *name)
{
  ((void)(__builtin_expect(!(argpos > 0), 0) ? __builtin_unreachable(), 0 : 0));
  if (argpos == 1)
      return 0;
  if (lst)
    lst->next = update_arglist_pass (lst->next, po, argpos - 1, name);
  else
    lst = update_arglist_pass (((void *)0), po, argpos - 1, name);
}
