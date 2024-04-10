/* PR middle-end/101195 */
/* { dg-do compile } */

int
foo (void)
{
  return __builtin_eh_return_data_regno (-42);
}
