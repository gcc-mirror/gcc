#include <stdint-gcc.h>

#define VMV_POS(TYPE,VAL)				\
  __attribute__ ((noipa))                               \
  void vmv_##VAL (TYPE dst[], int n)			\
  {                                                     \
    for (int i = 0; i < n; i++)                         \
      dst[i] = VAL;					\
  }

#define VMV_NEG(TYPE,VAL)				\
  __attribute__ ((noipa))                               \
  void vmv_m##VAL (TYPE dst[], int n)			\
  {                                                     \
    for (int i = 0; i < n; i++)                         \
      dst[i] = -VAL;					\
  }

#define TEST_ALL()	  \
VMV_NEG(int8_t,16)	  \
VMV_NEG(int8_t,15)    	  \
VMV_NEG(int8_t,14)    	  \
VMV_NEG(int8_t,13)    	  \
VMV_NEG(int16_t,12)       \
VMV_NEG(int16_t,11)       \
VMV_NEG(int16_t,10)       \
VMV_NEG(int16_t,9)	  \
VMV_NEG(int32_t,8)	  \
VMV_NEG(int32_t,7)	  \
VMV_NEG(int32_t,6)	  \
VMV_NEG(int32_t,5)	  \
VMV_NEG(int64_t,4)	  \
VMV_NEG(int64_t,3)	  \
VMV_NEG(int64_t,2)	  \
VMV_NEG(int64_t,1)	  \
VMV_POS(uint8_t,0)	  \
VMV_POS(uint8_t,1)	  \
VMV_POS(uint8_t,2)	  \
VMV_POS(uint8_t,3)	  \
VMV_POS(uint16_t,4)	  \
VMV_POS(uint16_t,5)	  \
VMV_POS(uint16_t,6)	  \
VMV_POS(uint16_t,7)	  \
VMV_POS(uint32_t,8)	  \
VMV_POS(uint32_t,9)	  \
VMV_POS(uint32_t,10)	  \
VMV_POS(uint32_t,11)	  \
VMV_POS(uint64_t,12)	  \
VMV_POS(uint64_t,13)	  \
VMV_POS(uint64_t,14)	  \
VMV_POS(uint64_t,15)	  \
VMV_POS(uint32_t,16)	  \
VMV_POS(uint32_t,123)     \
VMV_POS(uint32_t,255)     \
VMV_POS(uint32_t,999)     \
VMV_POS(uint32_t,32701)   \
VMV_POS(uint32_t,65535)   \
VMV_POS(uint32_t,65536)   \
VMV_POS(uint32_t,923423)  \

TEST_ALL()
