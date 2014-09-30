// { dg-do compile { target c++11 } }

struct A { };

template <class T>
decltype(f(T())) f(T t)		// { dg-error "depth" }
{
  return f(t);
}

int main()
{
  f(A());			// { dg-message "from here" }
}

// { dg-prune-output "compilation terminated" }
