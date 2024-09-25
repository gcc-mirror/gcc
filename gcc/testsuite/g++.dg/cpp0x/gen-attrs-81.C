// { dg-do compile { target c++11 } }

void
foo (int n)
{
  auto a = new int [n] [[gnu::deprecated]];				// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto b = new int [n] [[gnu::deprecated]] [42] [[]] [1] [[]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto c = new int [n] [[]] [42] [[gnu::deprecated]] [1] [[gnu::deprecated]];
  delete[] b;
  delete[] a;
}
