/* { dg-do compile } */

typedef __SIZE_TYPE__ size_t;

extern "C" {
    extern __inline __attribute__ ((__always_inline__))
    __attribute__ ((__gnu_inline__, __artificial__)) void *
    memcpy (void *__restrict __dest, __const void *__restrict __src,
	    size_t __len) throw ()
      {
	return __builtin___memcpy_chk (__dest, __src, __len,
				       __builtin_object_size (__dest, 0));
      }
}

typedef char TCODE[20];
typedef TCODE TCODE_ARRAY[5];
typedef struct PARAM
{
  TCODE_ARRAY tcode;
} PARAM;

static void foo (void* p)
{
  char buffer[4+sizeof(PARAM)];
  PARAM *param = (PARAM *)(buffer + 4);
  int i;

  for (i=0; i < 5; i++)
    {
      memcpy( param->tcode[i], p, 20 );
    }
}

void bar (void* p)
{
  foo (p);
}
