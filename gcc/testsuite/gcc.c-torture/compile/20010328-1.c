typedef __SIZE_TYPE__ size_t;
typedef unsigned int __u_int;
typedef unsigned long __u_long;

__extension__ typedef unsigned long long int __u_quad_t;
__extension__ typedef long long int __quad_t;

typedef struct
  {
    int __val[2];
  } __fsid_t;

typedef long int __blksize_t;
typedef long int __blkcnt_t;
typedef __quad_t __blkcnt64_t;
typedef __u_long __fsblkcnt_t;
typedef __u_quad_t __fsblkcnt64_t;
typedef __u_long __fsfilcnt_t;
typedef __u_quad_t __fsfilcnt64_t;
typedef __u_quad_t __ino64_t;

extern void *memcpy (void *__restrict __dest,
                     __const void *__restrict __src, size_t __n) ;

struct statfs
  {
    int f_type;
    int f_bsize;

    __fsblkcnt_t f_blocks;
    __fsblkcnt_t f_bfree;
    __fsblkcnt_t f_bavail;
    __fsfilcnt_t f_files;
    __fsfilcnt_t f_ffree;

    __fsid_t f_fsid;
    int f_namelen;
    int f_spare[6];
  };


struct statfs64
  {
    int f_type;
    int f_bsize;
    __fsblkcnt64_t f_blocks;
    __fsblkcnt64_t f_bfree;
    __fsblkcnt64_t f_bavail;
    __fsfilcnt64_t f_files;
    __fsfilcnt64_t f_ffree;
    __fsid_t f_fsid;
    int f_namelen;
    int f_spare[6];
  };

extern int __statfs (__const char *__file, struct statfs *__buf);
extern int __statfs64 (__const char *__file, struct statfs64 *__buf);


int
__statfs64 (const char *file, struct statfs64 *buf)
{
  struct statfs buf32;

  if (__statfs (file, &buf32) < 0)
    return -1;

  buf->f_type = buf32.f_type;
  buf->f_bsize = buf32.f_bsize;
  buf->f_blocks = buf32.f_blocks;
  buf->f_bfree = buf32.f_bfree;
  buf->f_bavail = buf32.f_bavail;
  buf->f_files = buf32.f_files;
  buf->f_ffree = buf32.f_ffree;
  buf->f_fsid = buf32.f_fsid;
  buf->f_namelen = buf32.f_namelen;
  memcpy (buf->f_spare, buf32.f_spare, sizeof (buf32.f_spare));

  return 0;
}
