typedef struct descriptor_dimension
{
  int stride;
  int lbound;
  int ubound;
} descriptor_dimension;
typedef struct {
    int *data;
    int dtype;
    descriptor_dimension dim[7];
} gfc_array_i4;

void
msum_i4 (gfc_array_i4 * const retarray,
	 gfc_array_i4 * const array,
	 const int * const pdim)
{
  int count[7];
  int extent[7];
  int * dest;
  const int * base;
  int dim;
  int n;
  int len;

  dim = (*pdim) - 1;
  len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;

  for (n = 0; n < dim; n++)
    {
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      count[n] = 0;
    }

  dest = retarray->data;
  base = array->data;

  do
    {
      int result = 0;

      for (n = 0; n < len; n++, base++)
	result += *base;
      *dest = result;

      count[0]++;
      dest += 1;
    }
  while (count[0] != extent[0]);
}

int main()
{
  int rdata[3];
  int adata[9];
  gfc_array_i4 retarray = { rdata, 265, { { 1, 1, 3 } } };
  gfc_array_i4 array = { adata, 266, { { 1, 1, 3 }, { 3, 1, 3 } } };
  int dim = 2;
  msum_i4 (&retarray, &array, &dim);
  return 0;
}
