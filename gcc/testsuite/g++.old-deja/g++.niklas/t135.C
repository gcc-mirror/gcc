// { dg-do compile }
// GROUPS niklas pt friend
template <class T> class C1
{
public:
	void diddle_C2 ();
};

class C2
{
	int data_member; // { dg-error "" }
  friend class C1; // { dg-error "" }
};

class C2 C2_object;

template <class T> void C1<T>::diddle_C2 ()
{
  C2_object.data_member = 99; // { dg-error "" }
}

C1<int> C1_int_object;

void foobar ()
{
  C1_int_object.diddle_C2 ();
}
