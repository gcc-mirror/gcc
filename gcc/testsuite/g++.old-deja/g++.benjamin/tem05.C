// 980924 bkoz
// just a quick test for export, to make sure we are warning for it.
// CHANGE ME when it's supported
// Build don't link: 


// 14 Templates
//p 6
// A namespace-scope declaration or definintion of a non-line function
// template, a non-inline member function template, a non-inline
// member function of a class template or a static data member of a
// class template may be preceeded by the export keyword. If such a
// template is defined in the same translation unit in which it is
// declared as exported, the definition is considered to be
// exported. The first declaration of the template containing the
// export keyword must not follow the definition. (meaning that export
// can't beredeclared as non-export??)

// 1
// template definition
export template <class T>  // WARNING - 
bool templ_one(T a) {
   if (a > 0)
     return true;
   else
     return false;
}


// 2
// static data, mf, mf template
template <class T>
class X_one {
  unsigned short	id;
  T	type;
public:
  static const bool 	is_specialized ;

  X_one(const unsigned short& us = 5): id(us), type(T(0)) {}
  unsigned short ret_id ();
  template <class T2> bool compare_ge(T2 test);
};

export template <class T> // WARNING - 
const bool X_one<T>::is_specialized = false;

export template <class T> // WARNING - 
unsigned short X_one<T>::ret_id() {
  return id;
}

export template <class T> template <class T2> // WARNING - 
bool compare_ge(T2 test) {
  if (test > type)
    return true;
  return false;
}

