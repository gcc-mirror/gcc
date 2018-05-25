// PR c++/68374
// { dg-options "-Wshadow" }

void foo ()
{
  static int i;  // { dg-message "shadowed declaration" }
  {
    int i;  // { dg-warning "shadows a previous local" }
  }
}
