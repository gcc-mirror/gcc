/* PR tree-optimization/33856 */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

typedef struct z_key
{
  int key;
  int mask;
} z_key;
typedef struct picture_size
{
  z_key key;
} picture_size;

void picture_size_new (picture_size *ps)
{
  z_key key;
  ps->key = key;
}

void picture_sizes_load_default (picture_size *ps)
{
  int i;
  for (i = 0; i < 5; ++i)
    picture_size_new (ps);
}
