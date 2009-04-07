// { dg-options "-fno-pretty-templates" }

template <class T, int N=0, int X=1>
struct A
{
};

void foo(void)
{
  A<void> a = 0;		// { dg-error "A<void, 0, 1>" }
}

template <class T> T f(T);	    // { dg-message "int f<int>.int." }
template <class T> T f(T, int = 0); // { dg-message "" }

int main()
{
  f(1);				// { dg-error "" }
}
