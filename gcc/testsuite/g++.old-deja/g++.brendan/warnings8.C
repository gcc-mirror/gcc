// { dg-do assemble  }
// { dg-options "-Wall" }
// GROUPS passed warnings
bool foo(unsigned char c)
{
  return (c & 17) != 0;
}
