// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 262 from Jeremy Sanders <jss@ast.cam.ac.uk>
// and several others. With templates, it's very easy to say something
// erroneous like
//    template class X::X<whatever>
// The culprit
//    ... class X::X ...
// caused us to ICE as we got confused about pushing and popping scopes.

template <class T> class image
{
public:   
  template <class U> image(const image<U> &copy);
};

template <class T> template <class U> image<T>::image(const image<U> &copy)
{
}

template class image<double>;
template class image<double>::image (const image<int> &); // { dg-error "" } parse error
template class image<double>::image (image<int>); // { dg-error "" } specified as declarator-id
template image<double>::image (const image<int> &);
