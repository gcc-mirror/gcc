/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fprofile-update=atomic -fprofile-generate -march=i386 -m32" } */

int main(int argc, char *argv[])
{
  return 0;
} /* { dg-warning "target does not support atomic profile update, single mode is selected" } */
