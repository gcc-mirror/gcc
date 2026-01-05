/* { dg-do run }  */
/* { dg-require-effective-target glibc }  */
/* { dg-options "-O3 -fno-math-errno -ftrapping-math -march=armv8-a+sve" }  */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <fenv.h>
#include <signal.h>

#pragma STDC FENV_ACCESS ON

__attribute__((noinline))
void f(float *__restrict c, int *__restrict d, int n)
{
    for (int i = 0; i < n; i++) {
        if (d[i] > 1000)
            c[i] = __builtin_sqrtf(c[i]);
    }
}

static void on_fpe(int sig)
{
    (void)sig;
    puts("SIGFPE: trapped FP exception (likely FE_INVALID from sqrt on a negative/sNaN lane)");
    fflush(stdout);
    __builtin_abort ();
}

int main(void)
{
    signal(SIGFPE, on_fpe);

    // Clear flags and enable trap on invalid operations.
    feclearexcept(FE_ALL_EXCEPT);
    feenableexcept(FE_INVALID);

    // Choose a length that is NOT a multiple of typical SVE VL (unknown at runtime),
    // and includes plenty of inactive lanes.
    const int n = 37;

    float *c = aligned_alloc(64, (size_t)n * sizeof(float));
    int   *d = aligned_alloc(64, (size_t)n * sizeof(int));
    if (!c || !d) return 1;

    // Construct data:
    // - For lanes where d<=1000, put negative values in c (sqrt would be FE_INVALID if executed).
    // - For lanes where d>1000, put positive values in c (legal sqrt).
    for (int i = 0; i < n; i++) {
        if ((i % 3) == 0) {
            d[i] = 1001;      // active
            c[i] = 4.0f;      // sqrt OK
        } else {
            d[i] = 0;         // inactive
            c[i] = -1.0f;     // sqrt would be invalid if wrongly executed
        }
    }

    // Call f. Correct behavior: no SIGFPE, and only positions with d>1000 are modified.
    f(c, d, n);

    // If traps are unavailable, at least report raised flags.
    int raised = fetestexcept(FE_ALL_EXCEPT);
    if (raised) {
        printf("FP flags raised: 0x%x\n", raised);
    } else {
        puts("No FP flags raised.");
    }

    // Check results.
    int ok = 1;
    for (int i = 0; i < n; i++) {
        if (d[i] > 1000) {
            if (!(c[i] == 2.0f)) { // sqrt(4) = 2
                printf("Mismatch at %d: expected 2.0, got %g\n", i, c[i]);
                ok = 0;
            }
        } else {
            if (!(c[i] == -1.0f)) { // must remain unchanged
                printf("Clobber at %d: expected -1.0 unchanged, got %g\n", i, c[i]);
                ok = 0;
            }
        }
    }

    puts(ok ? "OK" : "FAIL");
    free(c);
    free(d);
    return ok ? 0 : 2;
}

