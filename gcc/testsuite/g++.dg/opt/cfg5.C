/* PR rtl-optimization/34035 */
/* Origin: Janis Johnson <janis@gcc.gnu.org> */

/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -ffast-math -fno-gcse" } */

class One {
public:
    One () { e[0] = e[1] = 0.0; }
    double e[2];
};

template <class T>
class Two {
public:
    Two();
private:
    T *data;
    int arraySize;
};

template <class T>
Two<T>::Two() {
   data = new T[arraySize];
}

class Three {
protected:
  Two<One> data;
};

class Four : public Three {
public:
  Four ();
  void Foo (int n);
};

Four :: Four (){
   Foo (1);
}
