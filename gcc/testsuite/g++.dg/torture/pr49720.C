/* { dg-do compile } */

extern char t_start[], t_end[], t_size[];
bool foo (void)
{
  long size = reinterpret_cast<long>(t_size);
  return (size == t_end - t_start);
}
