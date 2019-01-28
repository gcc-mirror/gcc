struct Bar
 {
     typedef int type;
 };
 
 struct Foo
 {
     void func(void)
     {
       mutable Bar::type x; // { dg-error "8:non-member .x. cannot be declared .mutable." }
     }
 };
