/* { dg-do compile } */
/* { dg-options "-O3" } */

void generic_sendmsg (char *fmt, ...)
{
  __builtin_next_arg(fmt);
}

void generic_sendstat()
{
  double t;

  generic_sendmsg("F %3.2f", t);
}
