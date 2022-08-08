int open(const char *, int mode);
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2

typedef enum {
  S_IRWXU
  // etc
} mode_t;

int creat (const char *, mode_t mode);

void
test_1 (const char *path)
{
  int fd = open (path, O_RDONLY); /* { dg-message "\\(1\\) opened here" } */
  return; /* { dg-warning "leak of file descriptor 'fd' \\\[CWE-775\\\]" "warning" } */
 /* { dg-message "\\(2\\) 'fd' leaks here; was opened at \\(1\\)" "event" { target *-*-* } .-1 } */
}   

void
test_2 (const char *path)
{
  int fd = open (path, O_RDWR); /* { dg-message "\\(1\\) opened here" } */
  if (fd >= 0) /* { dg-message "\\(2\\) assuming 'fd' is a valid file descriptor" "event1" } */
  /* { dg-message "\\(3\\) following 'true' branch \\(when 'fd >= 0'\\)..." "event2" { target *-*-* } .-1 } */
  {
    return; /* { dg-warning "leak of file descriptor 'fd' \\\[CWE-775\\\]" "warning" } */
    /* { dg-message "\\(4\\) ...to here" "event1" { target *-*-* } .-1 } */
    /* { dg-message "\\(5\\) 'fd' leaks here; was opened at \\(1\\)" "event2" { target *-*-* } .-2 } */
  } 
}

void
test_3 (const char *path)
{
  int fd = open (path, O_WRONLY); /* { dg-message "\\(1\\) opened here" } */
  return; /* { dg-warning "leak of file descriptor 'fd' \\\[CWE-775\\\]" "warning" } */
}

void test_4 (const char *path)
{
  open(path, O_RDONLY); /* { dg-warning "leak of file descriptor \\\[CWE-775\\\]" } */
  /* { dg-message "\\(1\\) leaks here" "" { target *-*-* } .-1 } */
}

void 
test_5 (const char *path, mode_t mode)
{
  creat (path, mode); /* { dg-warning "leak of file descriptor \\\[CWE-775\\\]" } */
}

void
test_6 (const char *path, mode_t mode)
{
  int fd = creat (path, mode);
  return; /* { dg-warning "leak of file descriptor 'fd' \\\[CWE-775\\\]" } */
}


