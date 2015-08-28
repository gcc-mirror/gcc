/* { dg-do compile } */

typedef unsigned int size_t;
struct fde_vector
{
  size_t count;
  const struct dwarf_fde *array[];
};
struct object;
typedef struct dwarf_fde fde;
typedef int (*fde_compare_t) (struct object *, const fde *, const fde *);
void
fde_merge (struct object *ob, fde_compare_t fde_compare,
	   struct fde_vector *v1, struct fde_vector *v2)
{
  size_t i1, i2;
  const fde *fde2;
  do
    {
      i2--;
      while (i1 > 0 && fde_compare (ob, v1->array[i1 - 1], fde2) > 0)
	{
	  i1--;
	}
    }
  while (i2 > 0);
}
