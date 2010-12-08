// { dg-do assemble  }
template<bool B>
void f()			// { dg-message "note" }
{
}

int main()
{
  f<bool>(); // { dg-error "" } .*
  // { dg-message "candidate" "candidate note" { target *-*-* } 9 }
}

