// PRMS Id: 6826
// Check that unnecessary templates are not instantiated.

template <class T> 
class Test 
{ 
 public: 
  void doThiss(); 
  void doThat(); 
};

template <class T> 
void Test<T>::doThiss() 
{ 
  T x; 

  x.thiss(); 
} 

template <class T> 
void Test<T>::doThat() 
{ 
  T x; 

  x.that(); 
} 

class A 
{ 
 public: 
  void thiss() {}; 
};

class B
{ 
 public: 
  void that() {}; 
};

int main() 
{ 
  Test<A> a; 
  a.doThiss();			// a.doThat() is not well formed, but then
				// it's not used so needn't be instantiated. 
  
  Test<B> b;
  b.doThat();			// simillarly b.doThiss(); 
} 
