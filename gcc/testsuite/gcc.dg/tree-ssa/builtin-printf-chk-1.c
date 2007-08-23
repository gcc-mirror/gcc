/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab" } */

extern int __printf_chk (int, const char *, ...);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9, via;

void test (void)
{
  vi0 = 0;
  __printf_chk (1, "hello");
  vi1 = 0;
  __printf_chk (1, "hello\n");
  vi2 = 0;
  __printf_chk (1, "a");
  vi3 = 0;
  __printf_chk (1, "");
  vi4 = 0;
  __printf_chk (1, "%s", "hello");
  vi5 = 0;
  __printf_chk (1, "%s", "hello\n");
  vi6 = 0;
  __printf_chk (1, "%s", "a");
  vi7 = 0;
  __printf_chk (1, "%s", "");
  vi8 = 0;
  __printf_chk (1, "%c", 'x');
  vi9 = 0;
  __printf_chk (1, "%s\n", "hello\n");
  via = 0;
}

/* { dg-final { scan-tree-dump "vi0.*__printf_chk.*1.*\"hello\".*vi1" "fab"} } */
/* { dg-final { scan-tree-dump "vi1.*puts.*\"hello\".*vi2" "fab"} } */
/* { dg-final { scan-tree-dump "vi2.*putchar.*vi3" "fab"} } */
/* { dg-final { scan-tree-dump "vi3 ={v} 0\[^\(\)\]*vi4 ={v} 0" "fab"} } */
/* { dg-final { scan-tree-dump "vi4.*__printf_chk.*1.*\"hello\".*vi5" "fab"} } */
/* { dg-final { scan-tree-dump "vi5.*puts.*\"hello\".*vi6" "fab"} } */
/* { dg-final { scan-tree-dump "vi6.*putchar.*vi7" "fab"} } */
/* { dg-final { scan-tree-dump "vi7 ={v} 0\[^\(\)\]*vi8 ={v} 0" "fab"} } */
/* { dg-final { scan-tree-dump "vi8.*putchar.*vi9" "fab"} } */
/* { dg-final { scan-tree-dump "vi9.*puts.*\"hello\\\\n\".*via" "fab"} } */
/* { dg-final { cleanup-tree-dump "fab" } } */
