// Build don't link: 
// GROUPS passed arg-matching
// arg-matching file
// From: Terry Lee <terry@uivlsisd.csl.uiuc.edu>
// Date:     Sat, 14 May 1994 02:46:15 -0500
// Subject:  g++ 2.5.8 template<const void*> bug
// Message-ID: <199405140746.AA03993@uivlsisd.csl.uiuc.edu>

template<class T>
class A {
public:
    void func(const T& val) { }
};

int main()
{
    A<const void*> a;
    int* ptr = 0;
    a.func(ptr);
}
