// PR c++/14667

template<class T>
class Class1;

class Class2 {} typedef Class1<Class2> Type1; // { dg-error "" }
