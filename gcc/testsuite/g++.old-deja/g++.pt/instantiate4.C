// Build then link:

// Special g++ Options: -frepo -Werror

// Submitted by Melissa O'Neill <oneill@cs.sfu.ca>
// the vtable of Foo<int> wouldn't be generated

template <typename A>
struct Foo {
   virtual void foo() {}
};

template <typename A>
struct Bar {   
   void bar();
};

template <typename A> 
void Bar<A>::bar() {
   Foo<A> oof;
}

int main () {
    Bar<int> rab;
    
    rab.bar();
}
