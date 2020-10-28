/* PR c/94722 */
/* { dg-do compile } */

int __attribute__((no_stack_protector, stack_protect)) c() /* { dg-warning "ignoring attribute 'stack_protect' because it conflicts with attribute 'no_stack_protector'" } */
{
  return 0;
}
