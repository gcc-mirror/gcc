/* { dg-do compile } */
/* { dg-options "-g" } */

struct {
  unsigned long len;
  unsigned long size;
  char data[];
};			/* { dg-warning "unnamed struct" } */
struct {
  struct {
    unsigned long len;
    unsigned long size;
    char data[6];
  };
};			/* { dg-warning "unnamed struct" } */

