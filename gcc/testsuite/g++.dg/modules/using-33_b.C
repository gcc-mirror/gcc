// { dg-additional-options "-fmodules" }
import M;

using N::memset;

int main()
{
  int i = 1;
  memset (&i, 42, 1);
}
