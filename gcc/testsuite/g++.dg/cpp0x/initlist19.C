// { dg-do compile { target c++11 } }

// Allow other errors, too
// { dg-prune-output "error" }

void f(double);
int main()
{
  f({{1}});			// { dg-error "too many braces" }
}
