/* { dg-do compile } */

int __attribute__((optimize("no-lto"))) main(void){return 0;} /* { dg-warning "bad option" } */
