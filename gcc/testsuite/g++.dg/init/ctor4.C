// PR c++/17788
// { dg-do compile }

class foo {
public:
  foo();
};

class bar: public foo {	// { dg-error "uninitialized" }
		   // { dg-message "implicitly deleted" "" { target c++11 } 9 }
private:
  int &a; // { dg-message "should be initialized" }
};

foo::foo() {
}

int main(int argc, char **argv)
{
  bar x; // { dg-error "deleted" "" { target c++11 } }
         // { dg-message "synthesized" "" { target { ! c++11 } } 20 }
}
