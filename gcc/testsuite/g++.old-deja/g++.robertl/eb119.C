// { dg-do assemble  }
template<bool B>
void f()
{
}

int main()
{
  f<bool>(); // { dg-error "" } .*
}

