/* Reduced from an example in qemu-7.2.0: dump/win_dump.c  */

#include <stdbool.h>
#include <stdint.h>

extern int cpu_memory_rw_debug (bool x64, void *ptr);

int cpu_read_ptr(bool x64, uint64_t *ptr)
{
    int ret;
    uint32_t ptr32;
    uint64_t ptr64;

    ret = cpu_memory_rw_debug(x64, x64 ? (void *)&ptr64 : (void *)&ptr32);

    *ptr = x64 ? ptr64 : ptr32; /* { dg-bogus "use of uninitialized value" } */

    return ret;
}
