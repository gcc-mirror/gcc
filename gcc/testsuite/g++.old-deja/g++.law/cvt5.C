// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <1992Jul31.142856.10082@ericsson.se>
// From: jonas@ericsson.se (Jonas Nygren)
// Subject: g++ bug
// Date: Fri, 31 Jul 1992 14:28:56 GMT

class A {};
class B : public A {};

B b;

class R{
public:
  R() {}
  operator B&() { return b; }
};

void f(A&) {}

int main(){
  R r;

  f(r); // problem to cast to B& and then to A&
}


