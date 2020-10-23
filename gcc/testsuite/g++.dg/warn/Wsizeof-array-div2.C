// PR c++/91741
// { dg-do compile { target { lp64 } } }
// { dg-options "-Wall" }
// From <https://www.viva64.com/en/examples/v706/>.

const int kBaudrates[] = { 50, 75, 110 };

void
foo ()
{
  for(int i = sizeof(kBaudrates) / sizeof(char*); // { dg-warning "expression does not compute" }
      --i >= 0;)
    {
    }
}
