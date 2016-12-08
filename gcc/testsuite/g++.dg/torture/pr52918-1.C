// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
class bad_alloc   { };
typedef struct {
} __gthread_mutex_t;
int __gthread_mutex_unlock (__gthread_mutex_t *__mutex);
class __concurrence_unlock_error   {
};
inline void   __throw_concurrence_unlock_error()   {
    throw __concurrence_unlock_error();
}
class __mutex   {
    __gthread_mutex_t _M_mutex;
public:
    void unlock()     {
	if (__gthread_mutex_unlock(&_M_mutex) != 0)
	  __throw_concurrence_unlock_error();  	   
    }
};
class free_list   {
    typedef __mutex __mutex_type;
    __mutex_type&     _M_get_mutex();
    void _M_get(size_t __sz)
#if __cplusplus <= 201402L
    throw(bad_alloc)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
};
void  free_list::_M_get(size_t __sz)
#if __cplusplus <= 201402L
throw(bad_alloc)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  __mutex_type& __bfl_mutex = _M_get_mutex();
  __bfl_mutex.unlock();
  int __ctr = 2;
  while (__ctr)  {
      size_t* __ret = 0;
      --__ctr;
      try {
	  __ret = (size_t*) (::operator new(__sz + sizeof(size_t)));       
      }
      catch(const bad_alloc&) { }
  }
}
