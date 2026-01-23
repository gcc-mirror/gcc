/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv9-a" } */

#include <stdint.h>

struct
{
    int32_t f3;
    int64_t f4;
} g_137, g_138;
struct
{
    int8_t f0;
} g_174;
int32_t g_67;
extern uint32_t g_179[];
uint16_t func_71()
{
    for (; g_174.f0; g_174.f0 -= 1)
    {
        g_137.f3 = 0;
        for (; g_137.f3 <= 4; g_137.f3 += 1)
            for (g_67 = 3; g_67; g_67 -= 1) g_179[g_67] || (g_138.f4 = 0);
    }
}
