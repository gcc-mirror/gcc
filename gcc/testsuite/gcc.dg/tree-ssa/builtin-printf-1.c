/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab1" } */

extern int printf (const char *, ...);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9, via;

void test (void)
{
  vi0 = 0;
  printf ("hello");
  vi1 = 0;
  printf ("hello\n");
  vi2 = 0;
  printf ("a");
  vi3 = 0;
  printf ("");
  vi4 = 0;
  printf ("%s", "hello");
  vi5 = 0;
  printf ("%s", "hello\n");
  vi6 = 0;
  printf ("%s", "a");
  vi7 = 0;
  printf ("%s", "");
  vi8 = 0;
  printf ("%c", 'x');
  vi9 = 0;
  printf ("%s\n", "hello\n");
  via = 0;
}

/* { dg-final { scan-tree-dump "vi0.*printf.*\"hello\".*vi1" "fab1"} } */
/* { dg-final { scan-tree-dump "vi1.*puts.*\"hello\".*vi2" "fab1"} } */
/* { dg-final { scan-tree-dump "vi2.*putchar.*vi3" "fab1"} } */
/* { dg-final { scan-tree-dump "vi3 ={v} 0\[^\(\)\]*vi4 ={v} 0" "fab1"} } */
/* { dg-final { scan-tree-dump "vi4.*printf.*\"hello\".*vi5" "fab1"} } */
/* { dg-final { scan-tree-dump "vi5.*puts.*\"hello\".*vi6" "fab1"} } */
/* { dg-final { scan-tree-dump "vi6.*putchar.*vi7" "fab1"} } */
/* { dg-final { scan-tree-dump "vi7 ={v} 0\[^\(\)\]*vi8 ={v} 0" "fab1"} } */
/* { dg-final { scan-tree-dump "vi8.*putchar.*vi9" "fab1"} } */
/* { dg-final { scan-tree-dump "vi9.*puts.*\"hello\\\\n\".*via" "fab1"} } */
/* { dg-final { cleanup-tree-dump "fab1" } } */
