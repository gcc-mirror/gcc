// PR c++/9440
 struct A {
     explicit A(int = 0);
     A(const A&);
     operator int() const;
 };
 
 A
 bar(bool b, const A& a)
 {
     return (b ? A() : a);
 }
