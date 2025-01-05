// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110406
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-fno-moduleinfo -fdump-tree-optimized" }
struct cpuid_abcd_t
{
    uint eax;
    uint ebx;
    uint ecx;
    uint edx;
};

cpuid_abcd_t cpuid_insn(const uint in_eax)
{
    cpuid_abcd_t ret = void;
    asm { "cpuid"
        : "=a" (ret.eax),
          "=b" (ret.ebx),
          "=c" (ret.ecx),
          "=d" (ret.edx)
        : "a"  (in_eax)
        :;
    }
    return ret;
}
// { dg-final { scan-tree-dump-not "MEM <vector\\(4\\) uint>" "optimized" } }
