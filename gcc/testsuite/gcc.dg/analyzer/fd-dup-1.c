int open(const char *, int mode);
void close(int fd);
int dup (int old_fd);
int dup2 (int old_fd, int new_fd);
int dup3 (int old_fd, int new_fd, int flags);
int write (int fd, void *buf, int nbytes);
int read (int fd, void *buf, int nbytes);
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_ACCMODE 3

void test_1 (const char *path)
{
    int old_fd = open (path, O_RDWR);
    int new_fd = dup (old_fd); /* { dg-warning "'dup' on possibly invalid file descriptor 'old_fd'" } */
    close(old_fd);
    close(new_fd);
}

void test_2 (const char *path)
{
    int old_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd); 
        close(old_fd);
        return; /* { dg-warning "leak of file descriptor 'new_fd' \\\[CWE-775\\\]" } */
    }
}

void test_3 (const char *path, void *buf)
{
    int old_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd);
        write (new_fd, buf, 1); /* { dg-warning "'write' on possibly invalid file descriptor 'new_fd'" } */
        close (new_fd);
        close(old_fd);
    }
}


void test_5 (const char *path, void *buf)
{
    int old_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd);
        if (new_fd != -1)
        {
            write (new_fd, buf, 1); 
            close (new_fd);
            
        }
        close(old_fd);
    }
}


void test_7 (const char *path)
{
    int old_fd = open (path, O_RDWR);
    dup2 (old_fd, 4); /* { dg-warning "'dup2' on possibly invalid file descriptor 'old_fd'" } */
    close(old_fd);
}

void test_8 (const char *path)
{
    int old_fd = open (path, O_RDWR);
    int new_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        dup2 (old_fd, new_fd); /* { dg-warning "'dup2' on possibly invalid file descriptor 'new_fd'" } */
        close (old_fd);
    }
    close (new_fd);
}

void test_9 (const char *path, void *buf)
{
    int old_fd = open (path, O_RDWR);
    
    if (old_fd != -1)
    {
        int new_fd = open (path, O_RDWR);
        if (new_fd != -1)
        {
            int lhs = dup2 (old_fd, new_fd);
            write (lhs, buf, 1); /* { dg-warning "'write' on possibly invalid file descriptor 'lhs'" } */
            close(new_fd);
            close(lhs);
    }
        close(old_fd);        
    }
}

void test_10 (const char *path, int flags)
{
    int old_fd = open (path, O_RDWR);
    int new_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        dup3 (old_fd, new_fd, flags); /* { dg-warning "'dup3' on possibly invalid file descriptor 'new_fd'" } */
        close(old_fd);
        
    }
    close(new_fd);
}

void test_11 (const char *path, int flags)
{
    int old_fd = open (path, O_RDWR);
    int new_fd = open (path, O_RDWR);
    if (new_fd != -1)
    {
        dup3 (old_fd, new_fd, flags); /* { dg-warning "'dup3' on possibly invalid file descriptor 'old_fd'" } */
        close(new_fd);
        
    }
    close(old_fd);
}

void test_12 (const char *path, void *buf)
{
    int old_fd = open (path, O_RDONLY);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd);
        if (new_fd != -1)
        {
            write (new_fd, buf, 1); /* { dg-warning "'write' on read-only file descriptor 'new_fd'" } */
            close(new_fd);
        }
        close(old_fd);
    }
}

void test_13 (const char *path, void *buf)
{
    int old_fd = open (path, O_WRONLY);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd);
        if (new_fd != -1)
        {
            read (new_fd, buf, 1); /* { dg-warning "'read' on write-only file descriptor 'new_fd'" } */
            close(new_fd);
        }
        close(old_fd);
    }
}

void test_14 (const char *path, void *buf)
{
    int old_fd = open (path, O_RDWR);
    if (old_fd != -1)
    {
        int new_fd = dup (old_fd);
        if (new_fd != -1)
        {
            write (new_fd, buf, 1);
            read (new_fd, buf, 1);
            close(new_fd);
        }
        close(old_fd);
    }
}

void test_15 (void *buf)
{
    int fd = dup(0);
    read (fd, buf, 1); /* { dg-warning "'read' on possibly invalid file descriptor 'fd'" } */
    close(fd);
}

void test_16 (void *buf)
{
    int fd = dup(1);
    if (fd != -1)
    {
        write (fd, buf, 1);
        close (fd);
    }
}

void test_17 (const char *path)
{
    int fd = open (path, O_RDWR);
    close(fd);
    dup (fd); /* { dg-warning "'dup' on closed file descriptor 'fd'" }  */
    dup2 (fd, 4); /* { dg-warning "'dup2' on closed file descriptor 'fd'" }  */
}

void
test_18 (const char *path, void *buf)
{
    int fd = open (path, O_RDWR);
    if (fd != -1)
    {
        int fd2 = dup2 (fd, 3);
        read (fd2, buf, 1); /* { dg-warning "'read' on possibly invalid file descriptor 'fd2'" } */
        close(fd);
        close(fd2);
    }
}

void
test_19 (const char *path, void *buf)
{
    int fd = open (path, O_WRONLY);
    if (fd != -1)
    {
        int fd2 = dup2 (fd, 4);
        if (fd2 != -1)
        {
            read (fd2, buf, 1); /* { dg-warning "'read' on write-only file descriptor 'fd2'" } */
            close(fd2);
        }
        close (fd);
    }
    
}

extern int m;

void
test_20 ()
{
    int fd = dup (m); 
    close (fd);
}

void
test_21 ()
{
    int fd = dup2 (m, 1); 
    close (fd);
}

void
test_22 (int flags)
{
    int fd = dup3 (m, 1, flags);
    close (fd);
}

void do_something();
void
test_23 ()
{
    int nullfd = -1;
    int fd = 1;
    if (dup2 (nullfd, fd) < 0) /* { dg-warning "'dup2' on possibly invalid file descriptor 'nullfd'" } */
    {
        do_something();
    }
}

