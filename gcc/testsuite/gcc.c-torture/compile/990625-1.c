/* { dg-skip-if "no string.h in eBPF" { bpf-*-* } } */

#define __USE_STRING_INLINES
#include <string.h>

void test()
{
        char *p, *a;
        const char *s;

        while ( (s = a) )
          p = strcpy(strcpy(p,"/"), s);
}

