// { dg-do run }
/* { dg-additional-options "-std=c++14" } */
    typedef unsigned int u32;
    typedef unsigned char u8;

    static u32 pu8to32(const u8 * p8) __attribute__((noinline));
    static u32 pu8to32(const u8 * p8) {
      u32 v;
    
      __builtin_memcpy(&v, __builtin_assume_aligned(p8, 1), sizeof(u32));
    
      return v;
    }
    
    int main(void) {
      // dse1 throws this store away
      u8 d[sizeof(u32)] = {
          0x07, 0x00, 0x00, 0x07,
      };
    
      if (pu8to32(d) != 0x07000007)
        __builtin_trap();
    }
