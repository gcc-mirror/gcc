// { dg-additional-files "imports/pr109108.d" }
// { dg-additional-options "-I[srcdir] -fno-moduleinfo" }
// { dg-do link }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import imports.pr109108;

extern(C) int main()
{
    return test109108(0);
}
