// { dg-do assemble  }
// PRMS Id: 9500

template <int S> 
class base 
   { 
public: 
   inline base(); 
   }; 
 
template <class T> 
class derived : public base<sizeof(T)> 
   { 
public: 
   inline derived(); 
   }; 
 
template <class T> 
inline derived<T>::derived() : base<sizeof(T)>(){} 
