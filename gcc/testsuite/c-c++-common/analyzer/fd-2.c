int open(const char *, int mode);
void close(int fd);
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define STDIN 0

typedef enum {
  S_IRWXU
  // etc
} mode_t;

int creat (const char *, mode_t mode);

void 
test_1 (const char *path)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    close (fd); /* { dg-message "\\(2\\) first 'close' here" "event1" } */
    close (fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" "warning" } */
    /* { dg-message "\\(3\\) second 'close' here; first 'close' was at \\(2\\)" "event2" { target *-*-* } .-1 } */
}

void 
test_2 (const char *path)
{
    int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
    if (fd < 0) /* { dg-message "\\(2\\) assuming 'fd' is a valid file descriptor \\(>= 0\\)" "event1" } */
    /* { dg-message "\\(3\\) following 'false' branch \\(when 'fd >= 0'\\)..." "event2" { target *-*-* } .-1 } */
        return;
    close (fd); /* { dg-message "\\(4\\) ...to here" "event1" } */
    /* { dg-message "\\(5\\) first 'close' here" "event2" { target *-*-* } .-1 } */
    close (fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" "warning" } */
    /* {dg-message "\\(6\\) second 'close' here; first was at \\(5\\)" "" { target *-*-* } .-1 } */
}

void
test_3 ()
{
    /* FD 0 is stdin at the entry to "main" and thus read-only, but we have no
    guarantees here that it hasn't been closed and then reopened for
    writing, so we can't issue a warning */
    
    int fd = STDIN;
    close(fd); /* { dg-message "\\(1\\) first 'close' here" } */
    close(fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" "warning" } */
     /* { dg-message "\\(2\\) second 'close' here; first 'close' was at \\(1\\)" "event2" { target *-*-* } .-1 } */
}

void
test_4 ()
{
    int fd = -1;
    close(fd);
    close(fd);
}

void
test_5 (const char *path, mode_t mode)
{
    int fd = creat (path, mode);
    close(fd);
    close(fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" "warning" } */
}