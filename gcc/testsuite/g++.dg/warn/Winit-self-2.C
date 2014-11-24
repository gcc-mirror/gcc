// PR c++/63203
// { dg-options "-Winit-self" }

struct string { };

int main()
{
  for (int ii = 0; ii < 1; ++ii)
  {
    const string& str = str;  // { dg-warning "is initialized with itself" }
  }
}
