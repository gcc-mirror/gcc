/* { dg-options "-O2 -fdump-tree-vrp1 " } */

/* Tests that calls to update_stmt by the folder will also update ranger's
   cache value and produce the correct result for the builtin_constant_p
   function.  */

void dead ();

void foo( void *_thrdescr, int _result)
{
  const char *lossage = _result ? "constant string" : 0;

  if (__builtin_expect (lossage != ((void *)0) , 0))
    {
    unsigned __message_length = __builtin_strlen (lossage);
    if (! __builtin_constant_p (__message_length))
      dead ();
    }
}

/* { dg-final { scan-tree-dump-not "dead" "vrp1" } } */
