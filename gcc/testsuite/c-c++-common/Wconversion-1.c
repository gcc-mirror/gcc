/* { dg-do compile } */
/* { dg-options "-Wconversion -fno-trapping-math" } */

typedef char T;

void g()
{
  char c = 300; /* { dg-warning "conversion from .int. to .char. changes value from .300. to .44." } */
  T t = 300; /* { dg-warning "conversion from .int. to .T. {aka .char.} changes value from .300. to .44." } */
  signed char sc = 300; /* { dg-warning "conversion from .int. to .signed char. changes value from .300. to .44." } */
  unsigned char uc = 300; /* { dg-warning "conversion from .int. to .unsigned char. changes value from .300. to .44." } */
  unsigned char uc2 = 300u; /* { dg-warning "conversion from .unsigned int. to .unsigned char. changes value from .300. to .44." } */
  signed char c2 = (double)1.0 + 200; /* { dg-warning "overflow in conversion from .double. to .signed char. changes value from .2.01e\\+2. to .127." } */
}
