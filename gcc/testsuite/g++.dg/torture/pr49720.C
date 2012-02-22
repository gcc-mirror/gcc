/* { dg-do compile } */

__extension__ typedef __PTRDIFF_TYPE__ pdiff_t;

extern char t_start[], t_end[], t_size[];
bool foo (void)
{
  pdiff_t size = reinterpret_cast<pdiff_t>(t_size);
  return (size == t_end - t_start);
}
