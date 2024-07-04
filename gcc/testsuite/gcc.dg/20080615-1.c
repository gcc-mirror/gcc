/* { dg-do compile }  */
/* { dg-options "-w -fpermissive -O2" } */

static int *see_bb_splay_ar = ((void *) 0);
static void
see_merge_and_eliminate_extensions (void)
{
  int i = 0;
  printf ("* Phase 2: Merge and eliminate locally redundant extensions.  *\n");
  splay_tree_foreach (see_bb_splay_ar[i], ((void *) 0), ((void *) 0));
}
static void
see_main (void)
{
  int i = 0;
  see_merge_and_eliminate_extensions ();
  printf ("Searching register properties in bb %d\n", i);
}
gate_handle_see (void)
{
}
rest_of_handle_see (void)
{
  see_main ();
}
