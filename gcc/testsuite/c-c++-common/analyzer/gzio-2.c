void gzseek (long offset, int whence)
{
  if (whence == 2)
    return;
  if (whence == 0)
    offset -= 1;
  if (offset < 0)
    return;
  while (offset > 0) { /* { dg-warning "infinite loop" "" { xfail *-*-* } } */
  }
}
