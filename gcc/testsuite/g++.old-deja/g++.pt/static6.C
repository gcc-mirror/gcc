// { dg-do link }
// { dg-xfail-if "" { *-*-aout *-*-coff *-*-hpux* *-*-hms } { "*" } { "" } }

// Simplified from testcase by Erez Louidor Lior <s3824888@techst02.technion.ac.il>

template <class T> struct A {
 static const int l[1];
};

template<class T>
const int A<T>::l[1] = {1};

int i = A<int>::l[0];

int main(){}
