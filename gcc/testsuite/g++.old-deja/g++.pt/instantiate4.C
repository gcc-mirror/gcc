// { dg-do link }
// { dg-options "-frepo -Werror" }
// { dg-require-host-local "" }
// Build then link:


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

// { dg-final { cleanup-repo-files } }
