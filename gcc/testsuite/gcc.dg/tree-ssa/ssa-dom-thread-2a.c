/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom2-stats" } */

void bla();

/* In the following case, we should be able to thread edge through
   the loop header.  */

void thread_entry_through_header (void)
{
  int i;

  for (i = 0; i < 170; i++)
    bla ();
}

/* There's a single jump thread that should be handled by the VRP
   jump threading pass.  */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 1 "vrp1"} } */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 2" 0 "vrp1"} } */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "dom2"} } */
