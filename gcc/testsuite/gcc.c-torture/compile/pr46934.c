/* { dg-additional-options "-fpermissive" } */

int caller (unsigned int reg_type)
{
  switch (reg_type)
    {
    case 0x80000000:
      return (int)foo();

    case 0x80000003:
      return (int) bar();

    case 0x80000001:
      return (int) baz();

    case 0x80000004:
      return (int) fooz();
    }
}
