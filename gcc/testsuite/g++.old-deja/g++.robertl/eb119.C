// { dg-do assemble  }
template<bool B>
void f()			// { dg-message "candidate" }
{
}

int main()
{
  f<bool>(); // { dg-error "" } .*
}

