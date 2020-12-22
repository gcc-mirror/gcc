typedef signed char __v16qs __attribute__ ((__vector_size__ (16)));


inline auto
_mm_cmplt_epi8 (__v16qs __A, __v16qs __B)
{
  return __A < __B;
}
