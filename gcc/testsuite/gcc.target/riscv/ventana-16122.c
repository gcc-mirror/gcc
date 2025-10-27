/* { dg-do compile { target { rv64 } } } */

extern void NG (void);
typedef signed char int8_t;
typedef signed short int16_t;
typedef signed int int32_t;
void f74(void) {
    	int16_t x309 = 0x7fff;
	volatile int32_t x310 = 0x7fffffff;
	int8_t x311 = 59;
	int16_t x312 = -0x8000;
	static volatile int32_t t74 = 614992577;

    t74 = (x309==((x310^x311)%x312));

    if (t74 != 0) { NG(); } else { ; }
	
}

