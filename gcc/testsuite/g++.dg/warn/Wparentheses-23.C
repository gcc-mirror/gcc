// { dg-do compile }
// { dg-options "-Wparentheses" }

// Template version of Wparentheses-22.C.

int foo (int);

class C
{
 public:
  C()
    : b(0)
  { }

  // Use default assignment constructor.

  // Provide conversion to bool so that an instance of this class will
  // work as a condition.
  operator bool() const
  { return b != 0; }

 private:
  int b;
};

C a, b, c;
bool d;

template<class T>
void
bar (T)
{
  if (a = b) // { dg-warning "assignment" "correct warning" }
    foo (0);
  if ((a = b))
    foo (1);
  if (a = a) // { dg-warning "assignment" "correct warning" }
    foo (2);
  if ((a = a))
    foo (3);
  if (b = c) // { dg-warning "assignment" "correct warning" }
    foo (4);
  else
    foo (5);
  if ((b = c))
    foo (6);
  else
    foo (7);
  if (b = b) // { dg-warning "assignment" "correct warning" }
    foo (8);
  else
    foo (9);
  if ((b = b))
    foo (10);
  else
    foo (11);
  while (c = b) // { dg-warning "assignment" "correct warning" }
    foo (12);
  while ((c = b))
    foo (13);
  while (c = c) // { dg-warning "assignment" "correct warning" }
    foo (14);
  while ((c = c))
    foo (15);
  do foo (16); while (a = b); // { dg-warning "assignment" "correct warning" }
  do foo (17); while ((a = b));
  do foo (18); while (a = a); // { dg-warning "assignment" "correct warning" }
  do foo (19); while ((a = a));
  for (;c = b;) // { dg-warning "assignment" "correct warning" }
    foo (20);
  for (;(c = b);)
    foo (21);
  for (;c = c;) // { dg-warning "assignment" "correct warning" }
    foo (22);
  for (;(c = c);)
    foo (23);
  d = a = b; // { dg-warning "assignment" "correct warning" }
  foo (24);
  d = (a = b);
  foo (25);
  d = a = a; // { dg-warning "assignment" "correct warning" }
  foo (26);
  d = (a = a);
  foo (27);
  if (C(a))
    foo (28);
}

template<class T>
bool
bar1 (T)
{
  return a = b; // { dg-warning "assignment" "correct warning" }
}

template<class T>
bool
bar2 (T)
{
  return (a = b);
}

template<class T>
bool
bar3 (T)
{
  return a = a; // { dg-warning "assignment" "correct warning" }
}

template<class T>
bool
bar4 (T)
{
  return (a = a);
}

template void bar<int> (int); // { dg-message "required" }
template bool bar1<int> (int); // { dg-message "required" }
template bool bar2<int> (int);
template bool bar3<int> (int); // { dg-message "required" }
template bool bar4<int> (int);
