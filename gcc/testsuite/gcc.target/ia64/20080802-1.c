/* { dg-do compile } */
/* { dg-options "-O2 -msched-control-spec" } */

struct cpp_reader;

extern const char * parse_include (struct cpp_reader *, int *m, void *);
extern int _cpp_compare_file_date (struct cpp_reader *, const char *, int);

void
_cpp_init_internal_pragmas (struct cpp_reader *pfile)
{
  const char *fname;
  int angle_brackets, ordering;

  fname = parse_include (pfile, &angle_brackets, (void *) 0);
  if (!fname)
    return;
  ordering = _cpp_compare_file_date (pfile, fname, angle_brackets);
}
