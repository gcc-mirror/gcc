//980418 bkoz reduced from kumar's g++/15561
// Build don't link:
// prms-id: 15561

extern int errno;
extern int	write  (int, const void *, long unsigned int )  ;

/* to debug
#ifdef BUG
*/
//this crashes
typedef long unsigned int size_t;		
//typedef unsigned long size_t;
//this is ok
//typedef unsigned int size_t;

class exception {
public:
  exception () { }
  virtual ~exception () { }
  virtual const char* what () const;
};

class bad_alloc : public exception {
public:
  virtual const char* what() const throw() { return "bad_alloc"; }
};

struct nothrow_t {};
extern const nothrow_t nothrow;

typedef void (*new_handler)();
new_handler set_new_handler (new_handler);

void *operator new (size_t) throw (std::bad_alloc);
void *operator new[] (size_t) throw (std::bad_alloc);
void operator delete (void *) throw();
void operator delete[] (void *) throw();
void *operator new (size_t, const nothrow_t&) throw();
void *operator new[] (size_t, const nothrow_t&) throw();
void operator delete (void *, const nothrow_t&) throw();
void operator delete[] (void *, const nothrow_t&) throw();
inline void *operator new(size_t, void *place) throw() { return place; }
inline void *operator new[](size_t, void *place) throw() { return place; }

/* to debug
#else
#include <new>
#endif
*/

//from kumar's ace file
typedef	unsigned long	u_long;
typedef int ACE_thread_t;
typedef int ACE_hthread_t;
typedef int ACE_thread_key_t;
typedef int ssize_t;
typedef int ACE_HANDLE;
typedef ACE_HANDLE ACE_SOCKET;

struct ACE_OVERLAPPED
{
  u_long Internal;
  u_long InternalHigh;
  u_long Offset;
  u_long OffsetHigh;
  ACE_HANDLE hEvent;
};

struct strbuf {
	int	maxlen;			 
	int	len;			 
	char	*buf;			 
};

struct flock {
	short	l_type;		 
	short	l_whence;	 
	long	l_start;	 
	long	l_len;		 
	short	l_pid;		 
	short	l_xxx;		 
};

class   ACE_OS
{
public:
  struct ace_flock_t
  {
    void dump (void) const;
    struct flock lock_;
    ACE_HANDLE handle_;
  };
  static ssize_t write (ACE_HANDLE handle,
			const void *buf,
			size_t nbyte);
  static ssize_t write (ACE_HANDLE handle,
			const void *buf,
			size_t nbyte,
			ACE_OVERLAPPED *);

  static void *memcpy (void *t,
		       const void *s,
		       size_t len);

  static int putmsg (ACE_HANDLE handle,
		     const struct strbuf *ctl,
		     const struct strbuf *data,
		     int flags); 

  static ACE_thread_t NULL_thread;
  static ACE_hthread_t NULL_hthread;
  static ACE_thread_key_t NULL_key;
  static void mutex_lock_cleanup (void *mutex);
private:
  ACE_OS (void);
};


 
inline  ssize_t 
ACE_OS::write (ACE_HANDLE handle, const void *buf, size_t nbyte)
{
 do {   
	ssize_t  ace_result_ =   -1 ; 
	ace_result_ = ace_result_; 
	return  ::write (handle, buf, nbyte) ; } while (0) ;
}

inline  ssize_t 
ACE_OS::write (ACE_HANDLE handle, const void *buf, size_t nbyte,
	       ACE_OVERLAPPED *overlapped)
{
  overlapped = overlapped;
  return ACE_OS::write (handle, buf, nbyte);
}
  

inline  int 
ACE_OS::putmsg (ACE_HANDLE handle, const struct strbuf *ctl, 
		const struct strbuf *data, int flags) 
{
  {
    if (& flags ) 
      ;
  } ;
  if (ctl == 0 && data == 0)
    {
      errno = 22 ;
      return 0;
    }
   
  else if (ctl != 0)
    return ACE_OS::write (handle, ctl->buf, ctl->len);
  else if (data != 0)
    return ACE_OS::write (handle, data->buf, data->len);
  else
    {
      char *buf;
      do 
	{  
	  buf  = new   char [ctl->len + data->len] ; 
	  if ( buf  == 0) 
	    { 
	      errno = 12 ; 
	      return   -1 ;
	    } 
      	} 
      while (0) ;
      ACE_OS::memcpy (buf, ctl->buf, ctl->len);
      ACE_OS::memcpy (buf + ctl->len, data->buf, data->len);
      int result = ACE_OS::write (handle, buf, ctl->len + data->len);
      delete [] buf;
      return result;
    }
}

int main() 
{
  return (1);
}













