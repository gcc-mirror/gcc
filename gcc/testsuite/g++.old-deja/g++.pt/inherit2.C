// { dg-do assemble  }

// Make sure we make the right unqualified class a friend
// See PR c++/4403

template <class T> struct A
{
  struct AA;
  struct AC;
};

template <class T> class B
  :public A<T>
{
  friend struct B::AA;		// OK, this has an implicit typename
				// as if it is 'friend struct typename B::AA'
				// (I think there's a defect report
				// about that)
  friend struct AC;	// this makes ::AC a friend *not* A<T>::AC

  private: // only our friends can get out values
  static T valueA_AA;
  static T valueA_AC;
  static T value_AC;
};
template <typename T> T B<T>::valueA_AA;
template <typename T> T B<T>::valueA_AC;// { dg-message "" } private - 
template <typename T> T B<T>::value_AC;	// { dg-bogus "" }  - 

// this one is a friend
template <class T> struct A<T>::AA
{
  int M ()
  {
    return B<T>::valueA_AA;
  }
};

// this is not a friend
template <class T> struct A<T>::AC
{
  T M ()
  {
    return B<T>::valueA_AC;	// { dg-error "" } within this context - 
  }
};

// this is a friend
struct AC 
{
  int M ()
  {
    return B<int>::value_AC;	// { dg-bogus "" }  - 
  }
};

B<int> b;
A<int>::AA a_aa;
A<int>::AC a_ac;
AC ac;

int main ()
{
  a_aa.M ();
  a_ac.M ();
  ac.M ();
}
