/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */
extern inline void func1 (void) {
  static int i;  /* { dg-warning "static" } */
}
inline void func3 (void) 
{
  static int i;
}
