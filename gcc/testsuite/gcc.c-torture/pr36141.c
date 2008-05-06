extern void ffi_closure_unix (void);
ffi_prep_closure_loc (void)
{
  struct ia64_fd
  {
    unsigned long long code_pointer;
    unsigned long long gp;
  };
  struct ffi_ia64_trampoline_struct
  {
    unsigned long long code_pointer;
  };
  struct ffi_ia64_trampoline_struct *tramp;
  struct ia64_fd *fd;
  fd = (struct ia64_fd *)(void *)ffi_closure_unix;
  tramp->code_pointer = fd->code_pointer;
}
