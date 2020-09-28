void
longjmp (__SIZE_TYPE__, int);

void
e7 (__SIZE_TYPE__ gr)
{
  longjmp (gr, 1);
}
