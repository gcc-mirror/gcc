// PR c++/17788
// { dg-do compile }

class foo {
public:
  foo();
};

class bar: public foo {// { dg-error "uninitialized" }
private:
  int &a;
};

foo::foo() {
}

int main(int argc, char **argv)
{
  bar x; // { dg-error "synthesized" }
}
