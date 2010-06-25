/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-fnsplit" } */
int test2(a)
{
   if (a<100)
     return 1;
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   do_something_big ();
   return 0;
}

test()
{
  test2(10);
  test2(20);
}
/* { dg-final { scan-tree-dump-times "Splitting function" 1 "fnsplit"} } */
/* { dg-final { cleanup-tree-dump "fnsplit" } } */
