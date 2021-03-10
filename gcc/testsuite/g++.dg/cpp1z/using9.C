/* { dg-do compile { target c++17 } } */
// Created for c++ PR19377

class A2
{
  protected:
  int separate(int a);
  int separate(int a, int b);
  int separate(int a, int b, int c);
  int comma(int a);
  int alone;
};

class A1
{
  protected:
  int separate();
  int comma();
};

class A3
{
  protected:
  int comma(int a, int b);
};

class B:private A3, private A1, private A2
{
  // Using decls in a comma-separated list.
  using A2::comma, A3::comma, A1::comma;  // { dg-message "declared" }
  // Separate using statements.
  using A2::separate; // { dg-message "declared" }
  using A1::separate; // { dg-message "declared" }
  // No ambiguity, just for the sake of it.
  using A2::alone; // { dg-message "declared" }
};

class C:public B
{
  void f()
  {
    comma(); // { dg-error "private" }
    separate(); // { dg-error "private" }
    separate(1); // { dg-error "private" }
    separate(1, 2); // { dg-error "private" }
    separate(1, 2, 3); // { dg-error "private" }
    alone = 5; // { dg-error "private" }
  }
};
