int open(const char *, int mode);
void close(int fd);
int write (int fd, void *buf, int nbytes);
int read (int fd, void *buf, int nbytes);
int some_condition();

#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define STDIN 0
#define O_NOATIME 262144

void
test_1 (const char *path, void *buf)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    write (fd, buf, 1); /* { dg-message "\\(2\\) 'fd' could be invalid: unchecked value from \\(1\\)" } */
    /* { dg-warning "'write' on possibly invalid file descriptor 'fd'" "warning" { target *-*-* } .-1 } */
    close(fd);
}

void
test_2 (const char *path, void *buf)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    read (fd, buf, 1); /* { dg-message "\\(2\\) 'fd' could be invalid: unchecked value from \\(1\\)" } */
    /* { dg-warning "'read' on possibly invalid file descriptor 'fd'" "warning" { target *-*-* } .-1 } */
    close (fd);
}

void 
test_3 (void *buf)
{
    int fd = -1;
    read (fd, buf, 1); /* { dg-warning "'read' on possibly invalid file descriptor 'fd'" } */
    /* { dg-message "\\(1\\) 'fd' could be invalid" "" { target *-*-* } .-1 } */
}

void
test_4 (void *buf)
{
    int fd = STDIN;
    read (fd, buf, 1);
    close(fd);
}

void
test_5 (char *path, void *buf)
{
    int flags = O_RDONLY;
    if (some_condition())
        flags |= O_NOATIME;
    int fd = open (path, flags); /* { dg-message "\\(1\\) opened here" } */
    read (fd, buf, 1); /* { dg-warning "'read' on possibly invalid file descriptor 'fd'" } */
    /* { dg-message "\\(2\\) 'fd' could be invalid" "" { target *-*-* } .-1 } */
    close (fd);   
}


void
test_6 (char *path, void *buf)
{
    int fd = open (path, O_RDONLY);
    if (fd != -1)
    {
        read (fd, buf, 1);
    }
    close (fd);
}


void
test_7 (char *path, void *buf)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    if (fd != -1) /* { dg-message "\\(2\\) assuming 'fd' is an invalid file descriptor \\(< 0\\)" } */
    {
        read (fd, buf, 1);
    } else
    {
        write (fd, buf, 1); /* { dg-warning "'write' on possibly invalid file descriptor 'fd'" } */
        
    }
    close(fd);
}

void
test_read_from_symbolic_fd (int fd, void *buf)
{
  read (fd, buf, 1);
}

void
test_write_to_symbolic_fd (int fd, void *buf)
{
  write (fd, buf, 1);
}
