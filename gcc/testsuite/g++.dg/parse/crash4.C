struct Bar
 {
     typedef int type;
 };
 
 struct Foo
 {
     void func(void)
     {
       mutable Bar::type x; // { dg-error "" }
     }
 };
