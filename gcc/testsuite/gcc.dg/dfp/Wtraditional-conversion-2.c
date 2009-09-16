/* Test messages for -Wtraditional-conversion 
   (based on gcc.dg/Wtraditional-conversion-2.c).  */

/* { dg-do compile } */
/* { dg-options "-Wtraditional-conversion" } */

void fsi(signed int);
void fd32(_Decimal32);
void fd64(_Decimal64);
void fd128(_Decimal128);

struct s {
  void (*fsi)(signed int);
  void (*fd32)(_Decimal32);
  void (*fd64)(_Decimal64);
  void (*fd128)(_Decimal128);
} x;

signed int si;
unsigned int ui;
_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;

void
g (void)
{
  fsi(d32); /* { dg-warning "passing argument 1 of 'fsi' as integer rather than floating due to prototype" } */
  x.fsi(d32); /* { dg-warning "passing argument 1 of 'x.fsi' as integer rather than floating due to prototype" } */
  fsi(d64); /* { dg-warning "passing argument 1 of 'fsi' as integer rather than floating due to prototype" } */
  x.fsi(d64); /* { dg-warning "passing argument 1 of 'x.fsi' as integer rather than floating due to prototype" } */
  fsi(d128); /* { dg-warning "passing argument 1 of 'fsi' as integer rather than floating due to prototype" } */
  x.fsi(d128); /* { dg-warning "passing argument 1 of 'x.fsi' as integer rather than floating due to prototype" } */
  fd32(si); /* { dg-warning "passing argument 1 of 'fd32' as floating rather than integer due to prototype" } */
  x.fd32(si); /* { dg-warning "passing argument 1 of 'x.fd32' as floating rather than integer due to prototype" } */  
  fd64(ui); /* { dg-warning "passing argument 1 of 'fd64' as floating rather than integer due to prototype" } */
  x.fd64(ui); /* { dg-warning "passing argument 1 of 'x.fd64' as floating rather than integer due to prototype" } */
  fd128(si); /* { dg-warning "passing argument 1 of 'fd128' as floating rather than integer due to prototype" } */
  x.fd128(ui); /* { dg-warning "passing argument 1 of 'x.fd128' as floating rather than integer due to prototype" } */  
  fd32(1.0); /* { dg-warning "passing argument 1 of 'fd32' as '_Decimal32' rather than 'double' due to prototype" } */
  x.fd32(1.0); /* { dg-warning "passing argument 1 of 'x.fd32' as '_Decimal32' rather than 'double' due to prototype" } */
  fd64(1.0); /* { dg-warning "passing argument 1 of 'fd64' as '_Decimal64' rather than 'double' due to prototype" } */
  x.fd64(1.0); /* { dg-warning "passing argument 1 of 'x.fd64' as '_Decimal64' rather than 'double' due to prototype" } */
  fd128(1.0); /* { dg-warning "passing argument 1 of 'fd128' as '_Decimal128' rather than 'double' due to prototype" } */
  x.fd128(1.0); /* { dg-warning "passing argument 1 of 'x.fd128' as '_Decimal128' rather than 'double' due to prototype" } */
}
