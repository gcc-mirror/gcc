// Bug: g++ tries to instantiate ccList twice, and fails.
// Build don't link:

template<class T> class ccHandle{ };
template <class T> class ccList;
template <class T> class cc_List {
public:
  ccList <T>  copy ();
};

template <class T> class ccList : public ccHandle < cc_List <T> > {
public:
  ccList (int);
};

template <class T>
ccList <T> cc_List<T>::copy (){}

int main (int, char **) {
  ccList <int> size1();
}
