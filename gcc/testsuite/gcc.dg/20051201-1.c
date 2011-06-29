/* This failed with profiling due to a missing check in
   tree_flow_call_edges_add.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fprofile-generate -Wno-attributes" } */

static __attribute__ ((always_inline)) void 
baz ()
{
}

static __attribute__ ((always_inline)) int
bar ()
{
 out:
  baz ();
  goto out;
}

int
foo ()
{
  long res;
  
  res = bar ();
}
