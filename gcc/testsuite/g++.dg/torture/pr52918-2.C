// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
void*   __cxa_allocate_exception(size_t) throw();
typedef struct { } __gthread_mutex_t;
extern int __gthr_win32_mutex_unlock (__gthread_mutex_t *);
int __gthread_mutex_lock (__gthread_mutex_t *__mutex);
int __gthread_mutex_unlock (__gthread_mutex_t *__mutex);
void   __throw_concurrence_lock_error();
void   __throw_concurrence_unlock_error();
class __mutex   {
    __gthread_mutex_t _M_mutex;
public:
    void lock()     {
	if (__gthread_mutex_lock(&_M_mutex) != 0)
	  __throw_concurrence_lock_error();
    }
    void unlock()     {
	if (__gthread_mutex_unlock(&_M_mutex) != 0) 
	  __throw_concurrence_unlock_error();
    }
};
class __scoped_lock   {
    typedef __mutex __mutex_type;
    __mutex_type& _M_device;
public:
    explicit __scoped_lock(__mutex_type& __name) : _M_device(__name)     {
	_M_device.lock();
    }
    ~__scoped_lock() throw()    {
	_M_device.unlock();
    }
};
__mutex emergency_mutex;
void * __cxa_allocate_exception(size_t thrown_size) throw()
{
  void *ret;
  if (! ret)     
    __scoped_lock sentry(emergency_mutex);
  return 0;
}
