typedef struct {
    unsigned nargs;
} ffi_cif;
typedef struct {
    char tramp[24];
    ffi_cif *cif;
} ffi_closure;
extern void *memcpy (void *, const void *, __SIZE_TYPE__);
extern void ffi_closure_LINUX64 (void);

int
ffi_prep_closure_loc (ffi_closure *closure, ffi_cif *cif)
{
  void **tramp = (void **) &closure->tramp[0];

  memcpy (tramp, (char *) ffi_closure_LINUX64, 16);
  closure->cif = cif;

  return 0;
}
