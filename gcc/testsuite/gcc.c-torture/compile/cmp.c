struct fooalign {char x; double d;};
union fooround {long x; double d;};

int
foo ()
{
  int extra = 4;
  if (extra < sizeof (union fooround))
    extra = sizeof (union fooround);
  return extra;
}
