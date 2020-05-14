// { dg-do compile { target c++11 } }

struct iterator;
struct const_iterator {
  const_iterator(const iterator&);
  bool operator==(const const_iterator &ci) const = delete;
};
struct iterator {
  bool operator==(const const_iterator &ci) const {
    return ci == *this;		// { dg-error "deleted" "" { target c++17_down } }
  }				// { dg-warning "reversed" "" { target c++20 } .-1 }
};
