 
 
 
 
 
 

 
 
 
 




 

class ref_counted
{
 
protected:
        ref_counted( void ) : _count( 0 ) {}

public:
 
        unsigned int add_ref( void ) { return ++_count; }
        unsigned int release( void ) { return --_count; }
        unsigned int count( void ) const { return _count; }

 
protected:
        unsigned int _count;
};


 


template < class T >
class ref_ptr
{
 
public:
        ref_ptr( T* ptr = 0 ) : _ptr( ptr )
        {
                add_ref();
        }

        ref_ptr( const ref_ptr & rptr ) : _ptr( rptr.get() )
        {
                add_ref();
        }

        ~ref_ptr( void ) { release(); }

 
        T* get( void ) const { return _ptr; }
        T* operator->( void ) const { return get(); }
        T& operator*( void ) const { return *get(); }

        bool operator!( void ) const { return get() == 0; }
        bool operator==( const ref_ptr & rptr ) const { return *get() == *rptr;
}
        bool operator<( const ref_ptr & rptr ) const { return *get() < *rptr; }


        bool operator==( T* ptr ) const { return *get() == *ptr; }
        bool operator<( T* ptr ) const { return *get() < *ptr; }

        const ref_ptr & operator=( const ref_ptr & rptr )
        {
                release();
                _ptr = rptr.get();
                add_ref();

                return *this;
        }

       T* operator=( T* ptr )    
       {
         release();
         _ptr = ptr;
         add_ref();

         return _ptr;
       }

protected:
        void add_ref( void )
        {
                if( _ptr )
                        _ptr->add_ref();
        }

        void release( void )
        {
                if( _ptr && 0 == _ptr->release() )
                {
                        delete _ptr;
                        _ptr = 0;
                }
        }

 
protected:
        T *     _ptr;
};


template< class T >
bool operator==( T* ptr, const ref_ptr< T > & rptr )
{
        return *ptr == *rptr;
}

template< class T >
bool operator<( T* ptr, const ref_ptr< T > & rptr )
{
        return *ptr < *rptr;
}



class Baz : public ref_counted {
  int dummy;
};


class Bar;

int main() {
  ref_ptr<Baz> foo;
  static_cast<Bar *> (foo)->DoSomething;  //ERROR - invalid cast
}
