/* PR tree-optimization/89688 - -Wstringop-overflow confused by const 2D
   array of char
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-gimple -fdump-tree-optimized" } */

extern "C" __SIZE_TYPE__ strlen (const char*);

const char a2[2] = { '1' };

void a2_len ()
{
  if (strlen (a2) != 1)
    __builtin_abort ();
}

const char a2_2[2][3] = { { '1' }, { '1', '2' } };

void a2_2_len ()
{
  if  (strlen (a2_2[0]) != 1)   // { dg-bogus "-Wstringop-overflow" }
    __builtin_abort ();

  if  (strlen (a2_2[1]) != 2)   // { dg-bogus "-Wstringop-overflow" }
    __builtin_abort ();
}


/* { dg-final { scan-tree-dump-not "abort" "optimized" } }
   { dg-final { scan-tree-dump-not "strlen" "gimple" } } */
