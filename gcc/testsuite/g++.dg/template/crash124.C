// PR c++/50436

template <bool> struct VI {};
template <typename T>
struct IP
{
  static const bool r = IP<T*>::r;  // { dg-error "depth" }
};
template <typename T>
struct V
{
  static const bool r = IP<T*>::r;
  VI<r> vi;
};
struct X;
struct Y
{
  V<X> v;  // { dg-message "from here" }
}

// { dg-prune-output "compilation terminated" }
