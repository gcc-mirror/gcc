// Build don't link: 
// GROUPS passed arg-matching
typedef void* Ptr;


void func(int, const Ptr& p);

template <class T> void func(T, const Ptr& p);


Ptr& return_ref();


int main()
{
  char* x;

  func(x,return_ref());   // bug:
                          //         call of      func(int,  const Ptr&)
                          //         instead of   func(char*,const Ptr&)

}
