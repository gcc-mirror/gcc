// { dg-do assemble  }

template <class T>
void f(T t1, T t2);		// { dg-message "note" }

template <>
void f(int i, int j);

template <class T>
void g(T t1, T t2) {}		// { dg-message "note" }

template void g(int i, int j);

void h()
{
  f(3, 'c'); // { dg-error "" } no matching function
  // { dg-message "(candidate|deduced conflicting types)" "candidate note" { target *-*-* } 16 }
  g(3, 'c'); // { dg-error "" } no matching function
  // { dg-message "(candidate|deduced conflicting types)" "candidate note" { target *-*-* } 18 }
}


