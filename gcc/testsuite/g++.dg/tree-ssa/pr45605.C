/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-fre1" } */
extern "C" void abort(); 
bool destructor_called = false; 

struct B { 
    virtual void Run(){}; 
}; 

struct D : public B { 
    virtual void Run() 
      { 
        struct O { 
            ~O() { destructor_called = true; }; 
        } o; 

        struct Raiser { 
            Raiser()
#if __cplusplus <= 201402L
	    throw( int )			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
	    {throw 1;}; 
        } raiser; 
      }; 
}; 

int main() { 
    try { 
      D d; 
      static_cast<B&>(d).Run(); 
    } catch (...) {} 

    if (!destructor_called) 
      abort (); 
} 



/* We should devirtualize call to D::Run */
/* { dg-final { scan-tree-dump-times "D::Run \\(" 3 "fre1" } } */
