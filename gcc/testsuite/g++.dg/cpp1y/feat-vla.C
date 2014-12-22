// We shouldn't define this feature macro when we complain about VLAs.

#ifdef __cpp_runtime_arrays
#  error "__cpp_runtime_arrays"
#endif
