// { dg-do assemble  }
class A {};
class C {};

int main() {
  A* a = 0;
  C* c = static_cast<C*>(a);	// { dg-error "" } bad static cast 
}
