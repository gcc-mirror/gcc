// Build don't run:

// Simplified from testcase by Erez Louidor Lior <s3824888@techst02.technion.ac.il>

// excess errors test - XFAIL *-*-*

template <class T> struct A {
 static const int l[1];
};

template<class T>
const int A<T>::l[1] = {1};

int i = A<int>::l[0];
