// { dg-do assemble  }
// Origin: Loring Holden <lsh@cs.brown.edu> 

template <class T>
class REFptr {
   public:
      virtual ~REFptr();
      REFptr<T> &operator =  (const REFptr<T>& p);
};

class STR { };
class str_ptr : public REFptr<STR> { };

template <class T>
class ARRAY {
 protected:
   T      *_array; 
   int     _num;   
   int     _max;  
 public:
   virtual void realloc(int new_max) {
       _max = new_max;
       T *tmp = new T [_max];
       if (tmp == 0) return;
       for (int i=0; i<_num; i++) {
          tmp[i] = _array[i];
       }
       delete [] _array;
       _array = tmp;
   }
};

int
main()
{
   ARRAY<str_ptr> tags;
}
