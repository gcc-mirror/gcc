 template<class T>
 void f(const T&)
 {
         struct B {
	   
                 void g (T);
         };
         B b;
 };
 void g()
 {
         f(42);
 }
 
