// { dg-do run }

extern "C" void abort ();

void g (char c) 
{
  if (c != 'a')
    abort ();
}

void h (int i)
{
  if (i != 3)
    abort ();
}

template <typename T> void f(T const &t)
{
    union { char c; T t_; };

    c = 'a';
    g (c);
    t_ = 3;
    h (t_);
}

int main () {
  f (3);
}
