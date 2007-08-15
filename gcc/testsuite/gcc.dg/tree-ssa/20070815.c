/* { dg-do compile } */
/* { dg-options "-O -w" } */

/* This code snippet from glibc 2.3.5 was causing an ICE during
   optimization because we were failing to update the SSA form for
   stale name tags.  These are tags that are associated with SSA pointers
   that have been removed from the IL.  This was causing the SSA
   verifier to fail before we had a chance to run the cleanup pass that
   finally removes all the remaining PHI nodes for the stale name tag.  */
struct _IO_wide_data
{
};
struct _IO_FILE {
};
typedef struct _IO_FILE _IO_FILE;
struct _IO_jump_t
{
};
struct _IO_FILE_plus
{
  _IO_FILE file;
};
extern const struct _IO_jump_t _IO_file_jumps_maybe_mmap ;
extern const struct _IO_jump_t _IO_wfile_jumps;
extern const struct _IO_jump_t _IO_wfile_jumps_maybe_mmap ;

_IO_new_fdopen (fd, mode)
     const char *mode;
{
  int read_write;
  int posix_mode = 0;
  struct locked_FILE
  {
    struct _IO_FILE_plus fp;
    struct _IO_wide_data wd;
  } *new_f;
  int fd_flags;
  int use_mmap = 0;
    {
  }
    {
      switch (*++mode)
 {
 case '\0':
   use_mmap = 1;
 }
    }
  if (((fd_flags & 0003) == 00 && !(read_write & 8))
      || ((fd_flags & 0003) == 01 && !(read_write & 4)))
    {
    }
  if ((posix_mode & 02000) && !(fd_flags & 02000))
    return ((void *)0);
  _IO_no_init (&new_f->fp.file, 0, 0, &new_f->wd,
        (use_mmap && (read_write & 8))
        ? &_IO_wfile_jumps_maybe_mmap :
        &_IO_wfile_jumps);
    (use_mmap && (read_write & 8)) ? &_IO_file_jumps_maybe_mmap :
  _IO_file_init (&new_f->fp);
}
