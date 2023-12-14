  /* { dg-additional-options "-fanalyzer-verbose-state-changes" } */
int open(const char *, int mode);
void close(int fd);

#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2

void test_1 (const char* path)
{
    int fd = open (path, O_RDWR); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
    if (fd != -1)
    {
        close(fd); /* { dg-message "meaning: \\{verb: 'release', noun: 'resource'\\}" } */
        close(fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" } */
    }
}

void test_2 (const char* path)
{
    int fd = open (path, O_RDONLY); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
    if (fd != -1)
    {
        close(fd); /* { dg-message "meaning: \\{verb: 'release', noun: 'resource'\\}" } */
        close(fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" } */
    }
}

void test_3 (const char* path)
{
    int fd = open (path, O_WRONLY); /* { dg-message "meaning: \\{verb: 'acquire', noun: 'resource'\\}" } */
    if (fd != -1)
    {
        close(fd); /* { dg-message "meaning: \\{verb: 'release', noun: 'resource'\\}" } */
        close(fd); /* { dg-warning "double 'close' of file descriptor 'fd' \\\[CWE-1341\\\]" } */
    }
}