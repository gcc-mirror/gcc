// { dg-do assemble  }
// Origin: Andreas Kloeckner <ak@ixion.net>

template<class Iterator> struct iterator_traits {
  typedef typename Iterator::iterator_category 
  iterator_category; // { dg-error "" } no type iterator_category
};

template<class Category>
struct iterator {
  typedef Category  iterator_category;
};


template <class Iterator>
struct reverse_iterator : public // { dg-message "required" } no type iterator_category
iterator<typename iterator_traits<Iterator>::iterator_category> {
  protected:
  Iterator current;
  
};
class tag { };

template <class T>
struct list {
  template <class Item>
  struct list_iterator {
  };
  
  reverse_iterator<list_iterator<T> > rbegin()
    { return reverse_iterator<list_iterator<T> >
	(list_iterator<T>(Head->next())); } // { dg-error "" } not declared
};

template class list<int>;
