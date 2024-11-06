/* { dg-do compile } */
/* { dg-options "-std=gnu17 -pedantic" } */

enum err {
  err_IO = 0x8a450000, /* { dg-warning "int" } */
  err_NM, /* { dg-warning "int" } */
  err_EOF, /* { dg-warning "int" } */
  err_SE, /* { dg-warning "int" } */
  err_PT /* { dg-warning "int" } */
};
static enum err E_;
int error()
{
  switch (E_) {
    case err_IO : break; 
    case err_NM : break; 
    case err_EOF : break;
    case err_SE : break; 
    case err_PT : break; 
    default : return 0;
  }
}
