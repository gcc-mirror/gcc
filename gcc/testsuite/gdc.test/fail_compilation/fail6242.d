class A { void fun(int) {} }

class B : A { void fun(int x) in { assert(x > 0); } body {} }
