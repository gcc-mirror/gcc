// Build don't link:
// Origin: Andreas Kloeckner <ak@ixion.net>

template<class Iterator> struct iterator_traits {
  typedef typename Iterator::iterator_category 
  iterator_category; // ERROR - no type iterator_category
};

template<class Category>
struct iterator {
  typedef Category  iterator_category;
};


template <class Iterator>
struct reverse_iterator : public
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
    { return reverse_iterator<list_iterator<T> > // ERROR - no type
	(list_iterator<T>(Head->next())); } // ERROR - instantiated here
};

template class list<int>; // ERROR - instantiated from here
