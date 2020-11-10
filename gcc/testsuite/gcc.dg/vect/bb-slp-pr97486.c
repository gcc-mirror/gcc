/* { dg-do compile } */

struct {
  int *end_info;
  int *fp;
} png_load_body_c;

int *png_set_longjmp_fn();

void setjmp();

void png_load_body()
{
  int *fp;
  int png_ptr, info_ptr, *end_info;
  if (!fp)
    return;
  if (png_ptr) {
    info_ptr = 0;
    end_info = png_set_longjmp_fn();
  }
  png_load_body_c.end_info = end_info;
  png_load_body_c.fp = fp;
  if (png_ptr)
    png_set_longjmp_fn();
  setjmp(info_ptr);
}
