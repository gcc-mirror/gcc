/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab1" } */

typedef struct { int i; } FILE;
FILE *fp;
extern int fprintf (FILE *, const char *, ...);
volatile int vi0, vi1, vi2, vi3, vi4, vi5, vi6, vi7, vi8, vi9;

void test (void)
{
  vi0 = 0;
  fprintf (fp, "hello");
  vi1 = 0;
  fprintf (fp, "hello\n");
  vi2 = 0;
  fprintf (fp, "a");
  vi3 = 0;
  fprintf (fp, "");
  vi4 = 0;
  fprintf (fp, "%s", "hello");
  vi5 = 0;
  fprintf (fp, "%s", "hello\n");
  vi6 = 0;
  fprintf (fp, "%s", "a");
  vi7 = 0;
  fprintf (fp, "%c", 'x');
  vi8 = 0;
  fprintf (fp, "%d%d", vi0, vi1);
  vi9 = 0;
}

/* { dg-final { scan-tree-dump "vi0.*fwrite.*\"hello\".*1, 5, fp.*vi1" "fab1"} } */
/* { dg-final { scan-tree-dump "vi1.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi2" "fab1"} } */
/* { dg-final { scan-tree-dump "vi2.*fputc.*fp.*vi3" "fab1"} } */
/* { dg-final { scan-tree-dump "vi3 ={v} 0\[^\(\)\]*vi4 ={v} 0" "fab1"} } */
/* { dg-final { scan-tree-dump "vi4.*fwrite.*\"hello\".*1, 5, fp.*vi5" "fab1"} } */
/* { dg-final { scan-tree-dump "vi5.*fwrite.*\"hello\\\\n\".*1, 6, fp.*vi6" "fab1"} } */
/* { dg-final { scan-tree-dump "vi6.*fputc.*fp.*vi7" "fab1"} } */
/* { dg-final { scan-tree-dump "vi7.*fputc.*fp.*vi8" "fab1"} } */
/* { dg-final { scan-tree-dump "vi8.*fprintf.*fp.*\"%d%d\".*vi9" "fab1"} } */
/* { dg-final { cleanup-tree-dump "fab1" } } */
