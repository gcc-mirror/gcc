// PR c++/37016
// { dg-do run }
// { dg-options "-O2 -Wall" }

/*                                                                              
  Basic design concept is that WorldObject implements remote method call        
  functionality using the "curiously recurring template pattern" to enable      
  forwarding calls from the generic base class that implements the transport    
  layer to the derived class.                                                   

  The specific failure was in forwarding calls to items in a container.         
  This is emulated here by wrapping a single item.                              

  In the main program there are two forms of the call.  In the last             
  (uncommented) form the member function pointer is for clarity                 
  assigned to a variable (itemfunptr) before making the call.                   
  With 4.3.0 and 4.3.1 this code compiles incorrectly with -O1 or greater       
  to produce this warning                                                       

  reproduce.cc: In function ‘int main()’:                                       
  reproduce.cc:26: warning: ‘itemfunptr.void (Container::*)(void
(Item::*)(int), int)::__pfn’ is used uninitialized in this function             
  reproduce.cc:47: note: ‘itemfunptr.void (Container::*)(void (Item::*)(int),
int)::__pfn’ was declared here                                                  

  and the resulting executable segvs.  It works correctly with -O0.             

  With 4.2.3 and earlier it works correctly with optimization.                  

  In the first (commented out) form of the call in the main program             
  we directly refer to desired member function.  This compiles                  
  and executes correctly with all tested versions.                              
*/

extern "C" int printf (const char *, ...);

template <class Derived>
struct WorldObject {
    template <typename memfunT, typename arg1T, typename arg2T>
    void forward(memfunT memfun, arg1T arg1, arg2T arg2) {
        Derived* obj = static_cast<Derived*>(this);
        (obj->*memfun)(arg1, arg2);
    }
};

struct Item {
    void fred(int a) {
      printf ("a=%d\n", a);
    }
};

struct Container : public WorldObject<Container> {
    Item item;
    template <typename itemfunT, typename arg1T>
    void itemfun(itemfunT itemfun, int a) {
        (item.*itemfun)(a);
    }
};

int main() {
    typedef void (Item::*itemfun)(int);

    Container t;

    // This call compiles and executes correctly with all versions tested       
    //t.forward(&Container::itemfun<itemfun,int>, &Item::fred, 1);              

    // This call compiles with a warning and segvs on execution if using        
    // -O1 or greater with 4.3.*.  4.2.* is correct.                            
    void (Container::*itemfunptr)(itemfun, int) =
&Container::itemfun<itemfun,int>;
    t.forward(itemfunptr, &Item::fred, 1);

    return 0;
}

