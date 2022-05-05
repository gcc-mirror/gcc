/* Verify that -fanalyzer considers that mmfs escapes when passing either:
     *(mmfs + i)
   and
     (&mmfs[i])
   to an external function (for symbolic i).  */

typedef struct s_mmfile {
  char *ptr;
  long size;
} mmfile_t;

void init_mmfile(mmfile_t *ptr);

long test__init_via_ptr_arith__read_via_array_idx(int i)
{
  mmfile_t mmfs[3];
  init_mmfile(mmfs + i);
  return mmfs[i].size; /* { dg-bogus "uninit" } */
}

long test__init_via_array_idx__read_via_ptr_arith(int i)
{
  mmfile_t mmfs[3];
  init_mmfile(&mmfs[i]);
  return (mmfs + i)->size; /* { dg-bogus "uninit" } */
}

long test__ptr_arith_for_both(int i)
{
  mmfile_t mmfs[3];
  init_mmfile(mmfs + i);
  return (mmfs + i)->size; /* { dg-bogus "uninit" } */
}

long test__array_idx_for_both(int i)
{
  mmfile_t mmfs[3];
  init_mmfile(&mmfs[i]);
  return mmfs[i].size; /* { dg-bogus "uninit" } */
}
