// { dg-do compile }
// { dg-options "-fdump-tree-optimized" }

import gcc.attributes;

extern(C) @simd int simd_attr() { return 0; }

// { dg-final { scan-tree-dump "simd_attr\[ \\t\]simdclone|vector" "optimized" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbM4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } }

extern(C) @simd_clones("notinbranch") int simd_notinbranch() { return 0; }

// { dg-final { scan-tree-dump "simd_notinbranch\[ \\t\]simdclone|vector" "optimized" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4_simd_notinbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4_simd_notinbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8_simd_notinbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16_simd_notinbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVbM4_simd_notinbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVcM4_simd_notinbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVdM8_simd_notinbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVeM16_simd_notinbranch:" { target { i?86-*-* x86_64-*-* } } } }

extern(C) @simd_clones("inbranch") int simd_inbranch() { return 0; }

// { dg-final { scan-tree-dump "simd_inbranch\[ \\t\]simdclone|vector" "optimized" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVbN4_simd_inbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVcN4_simd_inbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVdN8_simd_inbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGVeN16_simd_inbranch:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbM4_simd_inbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4_simd_inbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8_simd_inbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16_simd_inbranch:" 1 { target { i?86-*-* x86_64-*-* } } } }
