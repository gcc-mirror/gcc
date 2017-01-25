int test_1 (int i, int j, int k)
{
  if (i < j)
    return k + 4;
  else
    return -k;
}

/* Example showing:
   - data structure
   - loop
   - call to "abort".  */

struct foo
{
  int count;
  float *data;
};

float test_2 (struct foo *lhs, struct foo *rhs)
{
  float result = 0.0f;

  if (lhs->count != rhs->count)
    __builtin_abort ();

  for (int i = 0; i < lhs->count; i++)
    result += lhs->data[i] * rhs->data[i];

  return result;
}
