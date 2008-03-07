// Test fix for PR20366
// 
// { dg-do compile  { target *-*-aix* } }
// { dg-options "-D_LARGE_FILES" }
//
// cstdio includes stdio.h and undefs most of the functions declared
// therein, unfortunately this means that #define fopen fopen64 goes
// away. This tests the fix, and ensures that with -D_LARGE_FILES
// fopen et. al. are indeed aliased to the large file equivalents.
//
// There are many other #define foo foo64 in the AIX headers, but
// these all work out fine as they are not undefined in libstdc++.
// This list is probably incomplete:
//
// Symbol          Return type     Large file declaration.
// 
// aio.h                      (different for different AIX versions)
// =====
// aio_read        int        aio_read64(int, struct aiocb64 *);
// aio_write       int        aio_write64(int, struct aiocb64 *);
// lio_listio      int        lio_listio64(int, struct liocb64 *[], int, void *);
// aio_cancel      int        aio_cancel64(int, struct aiocb64 *);
// aio_suspend     int        aio_suspend64(int, struct aiocb64 *[]);
// 
// stdio.h
// =======
// fgetpos         int        fgetpos64(FILE *, fpos64_t *);
// fopen           FILE      *fopen64(const char *, const char *);
// freopen         FILE      *freopen64(const char *, const char *, FILE *);
// fseeko          int        fseeko64(FILE *, off64_t, int);
// fsetpos         int        fsetpos64(FILE *, const fpos64_t *);
// ftello          off64_t    ftello64(FILE *);
// 
// unistd.h
// ========
// fclear          off64_t    fclear64(int, off64_t);
// fsync_range     int        fsync_range64(int, int, off64_t, off64_t);
// ftruncate       int        ftruncate64(int, off64_t);
// truncate        int        truncate64(const char *, off64_t);
// lseek           off64_t    lseek64(int, off64_t, int);
// pread           ssize_t    pread64(int, void *, size_t, off64_t);
// pwrite          ssize_t    pwrite64(int, const void *, size_t, off64_t);
// 
// fcntl.h
// =======
// open            int        open64(const char *, int, ...);
// creat           int        creat64(const char *, mode_t);
// 
// sys/stat.h
// ==========
// stat            int        stat64(const char *, struct stat64 *);
// fstat           int        fstat64(int, struct stat64 *);
// lstat           int        lstat64(const char *, struct stat64 *);
// 
// stdlib.h
// ========
// mkstemp         int        mkstemp64(char *);
// 
// ftw.h
// =====
// ftw             int        ftw64(const char *, int (*)(const char *,const struct stat64 *, int), int);
// nftw            int        nftw64(const char *, int (*)(const char *, const struct stat64 *, int, struct FTW*), int, int);
//
// It seems unlikely that any of these will be used (and #undef'ed) by
// libstdc++ in the future, if they are then this test and its
// associated patch to fixincludes will have to be revisited.

#include <cstdio>

extern "C" {
int        fgetpos(FILE *, fpos64_t *);
FILE      *fopen(const char *, const char *);
FILE      *freopen(const char *, const char *, FILE *);
int        fseeko(FILE *, off64_t, int);
int        fsetpos(FILE *, const fpos64_t *);
off64_t    ftello(FILE *);
}
int main() { 
  return 0;
}
