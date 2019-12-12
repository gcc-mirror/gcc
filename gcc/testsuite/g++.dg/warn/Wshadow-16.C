// PR c++/91868 - improve -Wshadow location.
// { dg-options "-Wshadow" }

int global; // { dg-message "shadowed declaration" }

struct S
{
  static int bar; // { dg-message "shadowed declaration" }
  S (int i) { int bar // { dg-warning "19:declaration of .bar. shadows a member" }
      (1);
    int global // { dg-warning "9:declaration of .global. shadows a global declaration" }
      (42);
  }
};

void
foo ()
{
  int xx; // { dg-message "shadowed declaration" }
  {
    S xx // { dg-warning "7:declaration of .xx. shadows a previous local" }
    (42);
  }
}
