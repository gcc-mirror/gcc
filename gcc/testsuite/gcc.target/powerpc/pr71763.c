// PR target/71763
// { dg-do compile }
// { dg-options "-O1 -mvsx" }
// { dg-xfail-if "PR70098" { lp64 && powerpc64_no_dm } }
// { dg-prune-output ".*internal compiler error.*" }

int a, b;
float c;

void fn2(void);

void fn1(void)
{
        long d;

        for (d = 3; d; d--) {
                for (a = 0; a <= 1; a++) {
                        b &= 1;
                        if (b) {
                                for (;;) {
                                        fn2();
                                        c = d;
                                }
                        }
                }
        }
}
