// PR c++/15269

struct B { 
    virtual int foo() __attribute__((deprecated)); 
}; 
 
int main(void) { 
  ((B*)0)->foo(); 		// { dg-warning "deprecated" }
} 
