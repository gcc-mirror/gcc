/* { dg-do compile } */
/* { dg-options "-O1" } */

/* The 100 calls alone do not cross the call_site_hash construction threshold
   in cgraph_node::get_edge, but together with the internal callback edges
   created for the assumes, they do.  We are testing whether the internal
   callback edges are added to call_site_hash.  */

#define CALL(x) (x) ()
#define CALL5(x) CALL (x), CALL (x), CALL (x), CALL (x), CALL (x)
#define CALL25(x) CALL5 (x), CALL5 (x), CALL5 (x), CALL5 (x), CALL5 (x)
#define CALL100(x) CALL25 (x), CALL25 (x), CALL25 (x), CALL25 (x)

bool foo ();

void
bar ()
{
  __attribute__ ((assume (foo ())));
  __attribute__ ((assume (foo ())));
  CALL100 (foo);
}
