// { dg-do compile { target concepts } }

template <class T> concept False = false;

template <class T>
struct A
{
  explicit operator bool ();
  explicit operator bool () requires False<T>;
};

int main()
{
  int i { A<int>() };		// { dg-error "" }
}
