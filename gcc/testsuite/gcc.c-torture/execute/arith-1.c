unsigned
sat_add (unsigned i)
{
  unsigned ret = i + 1;
  if (ret < i)
    ret = i;
  return ret;
}

main ()
{
  if (sat_add (~0U) != ~0U)
    abort ();
  exit (0);
}
