/* { dg-do compile } */
/* { dg-options "-pedantic" } */

enum err {
  err_IO = 0x8a450000, /* { dg-warning "int" } */
  err_NM,
  err_EOF,
  err_SE,
  err_PT
};
static enum err E_;
int error()
{
  switch (E_) {
    case err_IO : break;  /* { dg-warning "overflow" } */
    case err_NM : break;  /* { dg-warning "overflow" } */
    case err_EOF : break; /* { dg-warning "overflow" } */
    case err_SE : break;  /* { dg-warning "overflow" } */
    case err_PT : break;  /* { dg-warning "overflow" } */
    default : return 0;
  }
}
