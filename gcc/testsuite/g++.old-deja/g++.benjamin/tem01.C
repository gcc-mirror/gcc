// Build don't link: 
// prms-id: 13911

template<unsigned int N>
class ref_counter {
public:
  ref_counter() : p_refcnt(new unsigned int(N)) {}
  ref_counter(const ref_counter<N>& x) : p_refcnt(x.p_refcnt) { 
    ++*p_refcnt; 
  }
  ref_counter& operator=(const ref_counter<N>& rhs) {
    ++*rhs.p_refcnt;
    decrement();
    p_refcnt = rhs.p_refcnt;
    return *this;
  }
  ~ref_counter() {decrement();}
  
  bool unique() const {return *p_refcnt == N;}
  
private:
  unsigned int* p_refcnt;
  void decrement() {
    if (unique()) delete p_refcnt;
    else --*p_refcnt;
  }
};

template<class T, unsigned int N>
class ref_pointer {
public:
   
  ref_pointer() : the_p(0) {}  
  ref_pointer(T* just_newed) : the_p(just_newed) {}       
  virtual ~ref_pointer() {if (unique()) delete the_p;}
protected:
  ref_pointer(T* the_p_arg, ref_counter<N>& ref_count_arg)
    : the_p(the_p_arg), ref_count(ref_count_arg) {}               

public:
   
  ref_pointer& operator=(const ref_pointer&); 
  ref_pointer& operator=(T*);                         
  operator const T*() const {return the_p;}
  T* operator()() {return the_p;} 
  T* operator()() const {return the_p;} 
  T& operator*() const {return *the_p;}                       
  friend bool operator==(const ref_pointer<T, N>& lhs, 
			 const ref_pointer<T, N>& rhs) {
    return lhs.the_p == rhs.the_p;
  }
  friend bool operator!=(const ref_pointer<T, N>& lhs, 
			 const ref_pointer<T, N>& rhs) {
    return lhs.the_p != rhs.the_p;
  }
  
   
  bool unique() const {return ref_count.unique();}
  bool isNull() const {return the_p==0;}

protected:
  ref_counter<N>& refCount() {return ref_count;}

private:
   
  ref_counter<N> ref_count;
  T* the_p;
};

template<class T, unsigned int N>
ref_pointer<T, N>& ref_pointer<T, N>::operator=(const ref_pointer<T, N>& rhs) {
  if (the_p != rhs.the_p) {
    if (unique()) delete the_p;
    the_p = rhs.the_p;
    ref_count = rhs.ref_count;
  }
  return *this;
}


template<class T, unsigned int N>
ref_pointer<T, N>& ref_pointer<T, N>::operator=(T* just_newed) { 
  if (unique()) delete the_p;
  the_p = just_newed;
  ref_count = ref_counter<N>();
  return *this;
}



template<class T>
class CountedObjPtr : public ref_pointer<T, 1> {
public:
  CountedObjPtr() {}
  CountedObjPtr(T* just_newed) : ref_pointer<T, 1>(just_newed) {}
  CountedObjPtr(T* the_p_arg, ref_counter<1>& ref_count_arg)
    : ref_pointer<T, 1>(the_p_arg, ref_count_arg) {}
  CountedObjPtr<T>& operator=(T* rhs) {
    ref_pointer<T, 1>::operator=(rhs); 
    return *this; 
  }
  CountedObjPtr<T>& operator=(const CountedObjPtr<T>& rhs) { 
    ref_pointer<T, 1>::operator=(rhs); 
    return *this; 
  }
  T* operator->() const {return (*this)();}

};





//instantiating type

class TSObservable;

class TSObserver {
public:
   
  enum TSType { NormalTS, UpYldCrvTS, DownYldCrvTS, ZeroVolTS };
   
  virtual ~TSObserver() {}
   
  virtual void update(TSObservable* theChangedObservable) = 0;
  virtual TSType key() const { return myKey; }
  virtual TSType& key() { return myKey; }
protected:
  TSObserver(TSType myKeyArg) : myKey(myKeyArg) {}
  TSType myKey;
};



//now try to instantiate
template class CountedObjPtr<TSObserver>;
