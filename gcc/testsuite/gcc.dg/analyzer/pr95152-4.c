/* { dg-additional-options "-Wno-pointer-to-int-cast" } */
/* { C only: Wno-pointer-to-int-cast is not valid for C++. */

extern void my_func (int);
typedef struct {
  int var;
} info_t;
extern void *_data_offs;
void test()
{
  info_t *info = (info_t *) ((void *)((void *)1) + ((unsigned int)&_data_offs));
  my_func(info->var == 0);
}
