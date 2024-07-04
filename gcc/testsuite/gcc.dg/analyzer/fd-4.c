/* { dg-additional-options "-D_MODE_T_DECLARED=1" { target newlib } } */
/* { dg-additional-options "-D_DEFINED_mode_t" { target *-*-vxworks* } } */
#if defined(_AIX) || defined(__hpux)
#define _MODE_T
#endif
#include <stdio.h>

int open(const char *, int mode);
void close(int fd);
int write (int fd, void *buf, int nbytes);
int read (int fd, void *buf, int nbytes);

#define O_ACCMODE 0xf
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2

typedef enum {
  S_IRWXU
  // etc
} mode_t;

int creat (const char *, mode_t mode);

void
test_1 (const char *path, void *buf)
{
    int fd = open (path, O_RDONLY); /* { dg-message "opened here as read-only" } */
    if (fd >= 0) /* { dg-message "assuming 'fd' is a valid file descriptor \\(>= 0\\)" "event1" } */
    /* { dg-message "following 'true' branch \\(when 'fd >= 0'\\)..." "event2" { target *-*-* } .-1 } */
    {
        write (fd, buf, 1); /* { dg-warning "'write' on read-only file descriptor 'fd'" "warning" } */
        /* { dg-message "\\(4\\) ...to here" "event1" { target *-*-* } .-1 } */
        /* { dg-message "\\(5\\) 'write' on read-only file descriptor 'fd'" "event2" { target *-*-* } .-2 } */
        close (fd);
    }
}

void
test_2 (const char *path, void *buf)
{
    int fd = open (path, O_WRONLY); /* { dg-message "opened here as write-only" } */
    if (fd >= 0) /* { dg-message "assuming 'fd' is a valid file descriptor \\(>= 0\\)" "event1" } */
    /* { dg-message "following 'true' branch \\(when 'fd >= 0'\\)..." "event2" { target *-*-* } .-1 } */
    {
        read (fd, buf, 1); /* { dg-warning "'read' on write-only file descriptor 'fd'" "warning" } */
        /* { dg-message "\\(4\\) ...to here" "event1" { target *-*-* } .-1 } */
        /* { dg-message "\\(5\\) 'read' on write-only file descriptor 'fd'" "event2" { target *-*-* } .-2 } */
        close (fd);
    }
}


void 
test_3 (const char *path, void *buf)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    if (fd >= 0)
    {
        close(fd); /* {dg-message "\\(2\\) closed here"} */
        read(fd, buf, 1); /* { dg-warning "'read' on closed file descriptor 'fd'" }  */
        /* {dg-message "\\(3\\) 'read' on closed file descriptor 'fd'; 'close' was at \\(2\\)" "" {target *-*-*} .-1 } */
    }
}

void 
test_4 (const char *path, void *buf)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    if (fd >= 0)
    {
        close(fd); /* {dg-message "\\(2\\) closed here"} */
        write(fd, buf, 1); /* { dg-warning "'write' on closed file descriptor 'fd'" }  */
        /* {dg-message "\\(3\\) 'write' on closed file descriptor 'fd'; 'close' was at \\(2\\)" "" {target *-*-*} .-1 } */
    }
}

void
test_5 (const char *path)
{
    int fd = open (path, O_RDWR);
    close(fd);
    printf("%d", fd); /* { dg-bogus "'printf' on a closed file descriptor 'fd'" } */
}


void
test_6 (const char *path, mode_t mode, void *buf)
{
  int fd = creat (path, mode);
  if (fd != -1)
  {
    read (fd, buf, 1); /* { dg-warning "'read' on write-only file descriptor 'fd'" } */
    close(fd);
  }
}

void
test_7 (const char *path, mode_t mode, void *buf)
{
  int fd = creat (path, mode);
  if (fd != -1)
  {
    write (fd, buf, 1);
    close(fd);
  }
}
