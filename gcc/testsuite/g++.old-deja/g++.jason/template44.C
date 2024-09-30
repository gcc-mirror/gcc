// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for stdlib qsort" { ! hostedlib } }

#include <stdlib.h>
#include <string.h>

template <class T>
class List {
public:
    int len;
    T *array;

    int length() const { return( len ); }

    List() : len( 0 ), array( 0 ) {}
};

template <class T>
int AlgoStdCompare(const T* a, const T* b) {
  if (*a < *b)
    return -1;
  else
    return (*a > *b);	// 0 if equal, 1 if greater
}

int AlgoStdCompare(const char* const* a, const char * const*b)
{
    return strcmp(*a,*b);
}
     
template <class T>
void AlgoFixupSort(List< T >* , int, int ) {
}

template <class T> 
void AlgoSort(int (*compare)(const T *, const T *),
	  void (*fixup)( List<T> *, int first, int last),
	  List< T >* theList, int first, int last) {
  if (last < 0)
    last = theList->length()-1;
  
  qsort(theList->array+first, last-first+1, sizeof(T),
	(int (*)(const void *, const void *))compare);
  if (fixup)
    fixup(theList, first, last);
}

template <class T> 
void AlgoSort(List< T >* theList, int first = 0, int last = -1) {
  int (*compare)(const T*, const T*) = AlgoStdCompare;
  void (*fixup)( List<T> *, int first, int last) = AlgoFixupSort;
  
  AlgoSort(compare, fixup, theList, first, last);
}

int
main()
{
    List<const char *> slist;
    AlgoSort( &slist );

    List<int> ilist;
    AlgoSort( &ilist );
}
