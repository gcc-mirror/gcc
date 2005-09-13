/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab" } */

typedef struct { int i; } FILE;
FILE *fp;
extern int __fprintf_chk (FILE *, int, const char *, ...);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9;

void test (void)
{
  vi0 = 0;
  __fprintf_chk (fp, 1, "hello");
  vi1 = 0;
  __fprintf_chk (fp, 1, "hello\n");
  vi2 = 0;
  __fprintf_chk (fp, 1, "a");
  vi3 = 0;
  __fprintf_chk (fp, 1, "");
  vi4 = 0;
  __fprintf_chk (fp, 1, "%s", "hello");
  vi5 = 0;
  __fprintf_chk (fp, 1, "%s", "hello\n");
  vi6 = 0;
  __fprintf_chk (fp, 1, "%s", "a");
  vi7 = 0;
  __fprintf_chk (fp, 1, "%c", 'x');
  vi8 = 0;
  __fprintf_chk (fp, 1, "%d%d", vi0, vi1);
  vi9 = 0;
}

/* { dg-final { scan-tree-dump "vi0.*fwrite.*\"hello\".*1, 5, fp.*vi1" "fab"} } */
/* { dg-final { scan-tree-dump "vi1.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi2" "fab"} } */
/* { dg-final { scan-tree-dump "vi2.*fputc.*fp.*vi3" "fab"} } */
/* { dg-final { scan-tree-dump "vi3 = 0\[^\(\)\]*vi4 = 0" "fab"} } */
/* { dg-final { scan-tree-dump "vi4.*fwrite.*\"hello\".*1, 5, fp.*vi5" "fab"} } */
/* { dg-final { scan-tree-dump "vi5.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi6" "fab"} } */
/* { dg-final { scan-tree-dump "vi6.*fputc.*fp.*vi7" "fab"} } */
/* { dg-final { scan-tree-dump "vi7.*fputc.*fp.*vi8" "fab"} } */
/* { dg-final { scan-tree-dump "vi8.*__fprintf_chk.*fp.*1.*\"%d%d\".*vi9" "fab"} } */
/* { dg-final { cleanup-tree-dump "fab" } } */
