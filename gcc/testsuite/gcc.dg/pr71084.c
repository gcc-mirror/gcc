/* PR tree-optimization/71084 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void babl_format (void);
void gimp_drawable_get_format (void);
int _setjmp (void);

enum {
  GIMP_RGB_IMAGE,
  GIMP_RGBA_IMAGE,
  GIMP_GRAY_IMAGE,
  GIMP_GRAYA_IMAGE,
  GIMP_INDEXED_IMAGE
} run_i;

int run_height;

void fn1 ()
{
  int type, width;
  if (_setjmp ())
    switch (type)
      {
      case GIMP_RGB_IMAGE:
	babl_format ();
      case GIMP_RGBA_IMAGE:
      case GIMP_GRAY_IMAGE:
	babl_format ();
      case GIMP_GRAYA_IMAGE:
      case GIMP_INDEXED_IMAGE:
	gimp_drawable_get_format();
      }
  for (; run_height;)
    for (; run_i < (long)fn1; ++run_i)
      for (; width;)
        ;
}
