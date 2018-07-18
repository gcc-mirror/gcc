// PR 59930 (part)  templated class friend declarations cannot have
// default args.

template <class T>
struct S
{ 
  template <class U = int> friend class R; // { dg-error "template friend" }
  template <class U = int> friend class S; // { dg-error "template friend" }
};
