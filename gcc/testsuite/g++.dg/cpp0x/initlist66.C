// PR c++/52510
// { dg-do compile { target c++11 } }

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef uint64_t upad64_t;

typedef struct _pthread_cond {
 struct {
  uint8_t __pthread_cond_flag[4];
  uint16_t __pthread_cond_type;
  uint16_t __pthread_cond_magic;
 } __pthread_cond_flags;
 upad64_t __pthread_cond_data;
} pthread_cond_t;

class gtm_rwlock
{
  pthread_cond_t c_readers;
 public:
  gtm_rwlock();
};

gtm_rwlock::gtm_rwlock()
  : c_readers ({{{0, 0, 0, 0}, 0, 0x4356}, 0})
{ }

