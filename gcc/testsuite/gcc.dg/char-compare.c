/* PR rtl-optimization/23241 */
/* Origin: Josh Conner <jconner@apple.com> */

/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

struct fbs {
  unsigned char uc;
} fbs1 = {255};

void fn(struct fbs *fbs_ptr)
{
  if ((fbs_ptr->uc != 255) && (fbs_ptr->uc != 0))
    abort();
}

int main(void)
{
  fn(&fbs1); 
  return 0;
}
