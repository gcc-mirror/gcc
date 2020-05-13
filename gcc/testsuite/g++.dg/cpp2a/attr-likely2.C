// { dg-do compile { target c++20 } }

bool b;
int main()
{
  if (b)
    [[likely, likely]] b;	// { dg-warning "ignoring" }
  else
    [[unlikely]] [[likely]] b;	// { dg-warning "ignoring" }

  [[likely, unlikely]] lab:;	// { dg-warning "ignoring" }
}
