/* PR middle-end/87052 - STRING_CST printing incomplete in Gimple dumps
   { dg-do compile }
   { dg-options "-fdump-tree-gimple" } */

void sink (const void*, ...);

void test (void)
{
  const char a[3] = "\000ab";

  /* Expect the following in the dump:
     a = "\x00ab"; */

  const char b[] = { 'a', 0, 'b', 'c' };

  /* Expect the following:
     b = "a\x00bc"; */

  const char c[] = "";

  /* Expect the following:
     c = ""; */

  const char d[0] = { };

  /* Expect nothing.  */

  const char e[0] = "";

  /* Expect nothing.  */

  sink (a, b, c, d, e);
}

/* { dg-final { scan-tree-dump-times "a = \"\\\\x00ab\";" 1 "gimple" } }
   { dg-final { scan-tree-dump-times "b = \"a\\\\x00bc\";"  1 "gimple" } }
   { dg-final { scan-tree-dump-times "c = \"\";"  1 "gimple" } }
   { dg-final { scan-tree-dump-times "d = "  1 "gimple" } }
   { dg-final { scan-tree-dump-times "d = {CLOBBER\\(eos\\)}"  1 "gimple" } }
   { dg-final { scan-tree-dump-times "e = "  1 "gimple" } }
   { dg-final { scan-tree-dump-times "e = {CLOBBER\\(eos\\)}"  1 "gimple" } }  */
