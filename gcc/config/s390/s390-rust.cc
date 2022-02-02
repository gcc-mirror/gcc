/* Subroutines for the Rust front end for the IBM S/390 and zSeries architectures.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tm_p.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for S/390 and zSeries targets.  */

void s390_rust_target_cpu_info(void) {
    // TODO: ensure that this is right for llvm/rustc arch 
    if (TARGET_64BIT)
        rust_add_target_info("target_arch", "s390x");
    else
        rust_add_target_info("target_arch", "s390");

    // names derived from llvm and rustc
    if (TARGET_SOFT_FLOAT) 
        rust_add_target_info("target_feature", "soft-float");
    else
        rust_add_target_info("target_feature", "fp-extension");
    // TODO: ensure that having hardware float is actually what fp-extension refers to
    /* TODO: find gcc equivalent of distinct-ops (distinct-operands facility), fast-serialization, 
     * high-word, interlocked-access1, load-store-on-cond, population-count, 
     * message-security-assist-extension3, message-security-assist-extension4, 
     * reset-reference-bits-multiple, execution-hint, load-and-trap, miscellaneous-extensions, 
     * processor-assist, dfp-zoned-conversion, enhanced-dat-2, load-and-zero-rightmost-byte, 
     * load-store-on-cond-2, message-security-assist-extension5, dfp-packed-conversion, 
     * miscellaneous-extensions-2, message-security-assist-extension7, message-security-assist-extension8, 
     * vector-enhancements-1, vector-packed-decimal, insert-reference-bits-multiple, 
     * miscellaneous-extensions-3, message-security-assist-extension9, vector-enhancements-2, 
     * vector-packed-decimal-enhancement, enhanced-sort, deflate-conversion if they exist */    
    if (TARGET_OPT_HTM)
        rust_add_target_info("target_feature", "transactional-execution");
    // TODO: ensure that "vector" only refers to code generation and not language exts and builtins
    if (TARGET_OPT_VX)
        rust_add_target_info("target_feature", "vector");
    // TODO: is guarded-storage the same thing as stack-guard? if so, add that option mapping
}
