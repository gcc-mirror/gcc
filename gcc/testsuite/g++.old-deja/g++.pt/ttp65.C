// Build don't link:

// Copyright (C) 2000 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Bug: We used reject template unification of two bound template template
// parameters.

template <class T, class U=int> class C
{
};

template <class T, class U> void f(C<T,U> c)
{
}

template <class T> void f(C<T> c)
{
}

template <template<class,class=int> class C, class T, class U>
void g(C<T,U> c)
{
}

template <template<class,class=int> class C, class T> void g(C<T> c)
{
}

int main()
{
  C<int,char> c1;
  f(c1);
  g(c1);
  C<int,int> c2;
  f(c2);
  g(c2);
}
