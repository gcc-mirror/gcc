// GROUPS passed code-generation
// execution test - XFAIL *-*-*
// code-gen file
// From: mscha@anne.wifo.uni-mannheim.de (Martin Schader)
// Date:     Wed, 4 Aug 93 19:14:52 +0200
// Message-ID: <9308041714.AA00752@anne.wifo.uni-mannheim.de>

extern "C" int printf (const char *, ...);

template<class T> struct Y {
    Y* next;
};

template<class T> struct X {
    X() { ptrY = 0; }
    void f();
    Y<T>* ptrY;
};

template<class T> void X<T>::f() {
    ptrY->next = ptrY = new Y<T>;
//
//  Use two assignment statements and it works
//    ptrY = new Y<T>;
//    ptrY->next = ptrY;
}

int main() {
    X<int> x;
    x.f();
    printf ("PASS\n");
    exit(0);
}
