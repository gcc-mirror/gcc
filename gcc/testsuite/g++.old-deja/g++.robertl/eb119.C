// { dg-do assemble  }
template<bool B>
void f()			// { dg-message "note" }
{
}

int main()
{
  f<bool>(); // { dg-error "" } .*
}

