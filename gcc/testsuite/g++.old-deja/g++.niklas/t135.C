// Build don't link: 
// GROUPS niklas pt friend
// excess errors test - XFAIL *-*-*
template <class T> class C1
{
public:
	void diddle_C2 ();
};

class C2
{
	int data_member;
        friend class C1;
};

class C2 C2_object;

template <class T> void C1<T>::diddle_C2 ()
{
  C2_object.data_member = 99;
}

C1<int> C1_int_object;

void foobar ()
{
  C1_int_object.diddle_C2 ();
}
