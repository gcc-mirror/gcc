// { dg-do run  }
// Origin: Ewgenij Gawrilow <gawrilow@math.TU-Berlin.DE>

extern "C" void abort ();

template <template <class X> class B, class A>
struct is_instance_of {
   enum { answer=false };
};

template <template <class X> class B, class T>
struct is_instance_of<B, B<T> > {
   enum { answer=true };
};

template <class X> struct C { };
template <class X> struct D { };

template <class T>
bool is_C (const T&) {
   return is_instance_of<C,T>::answer;
}

int main() {
   if (!is_C(C<int>()))
      abort ();
   if (is_C(D<int>()))
      abort ();
   return 0;
}
