// PR c++/14369

struct A { };

template<class T> 
struct X : A {
   const A* bar() const
   { return this; }

   const A& foo() const;
};

template<class T>
const A& X<T>::foo() const
{
   const A* t = bar();
   return *(t ? t : throw 0);
}


