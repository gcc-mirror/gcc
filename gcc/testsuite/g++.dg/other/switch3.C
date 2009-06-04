// PR c++/39371
// { dg-do compile }

void
foo (bool b)
{
  switch ((unsigned int) b)
    {
    case 1:
    case 2:
      break;
    }
}

void
bar (unsigned char b)
{
  switch ((unsigned int) b)
    {
    case 1:
    case 257:
    case 513:
      break;
    }
}
