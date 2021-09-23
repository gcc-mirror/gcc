#!/usr/bin/env python3

# Copyright (C) 2020 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# This script parses a .diff file generated with 'diff -up' or 'diff -cp'
# and adds a skeleton ChangeLog file to the file. It does not try to be
# too smart when parsing function names, but it produces a reasonable
# approximation.
#
# Author: Martin Liska <mliska@suse.cz>

import unittest

from mklog import generate_changelog

import unidiff

unidiff_supports_renaming = hasattr(unidiff.PatchedFile(), 'is_rename')


PATCH1 = '''\
diff --git a/gcc/config/riscv/riscv.h b/gcc/config/riscv/riscv.h
index 567c23380fe..e6209ede9d6 100644
--- a/gcc/config/riscv/riscv.h
+++ b/gcc/config/riscv/riscv.h
@@ -920,6 +920,7 @@ extern unsigned riscv_stack_boundary;
 #define SHIFT_RS1 15
 #define SHIFT_IMM 20
 #define IMM_BITS 12
+#define C_S_BITS 5
 #define C_SxSP_BITS 6
 
 #define IMM_REACH (1LL << IMM_BITS)
@@ -929,6 +930,10 @@ extern unsigned riscv_stack_boundary;
 #define SWSP_REACH (4LL << C_SxSP_BITS)
 #define SDSP_REACH (8LL << C_SxSP_BITS)
 
+/* This is the maximum value that can be represented in a compressed load/store
+   offset (an unsigned 5-bit value scaled by 4).  */
+#define CSW_MAX_OFFSET ((4LL << C_S_BITS) - 1) & ~3
+
 /* Called from RISCV_REORG, this is defined in riscv-sr.c.  */
 
 extern void riscv_remove_unneeded_save_restore_calls (void);

'''

EXPECTED1 = '''\
gcc/ChangeLog:

	* config/riscv/riscv.h (C_S_BITS):
	(CSW_MAX_OFFSET):

'''

PATCH2 = '''\
diff --git a/gcc/targhooks.h b/gcc/targhooks.h
index 9704d23f1db..b572a36e8cf 100644
--- a/gcc/targhooks.h
+++ b/gcc/targhooks.h
@@ -120,7 +120,7 @@ extern bool default_empty_mask_is_expensive (unsigned);
 extern void *default_init_cost (class loop *);
 extern unsigned default_add_stmt_cost (class vec_info *, void *, int,
 				       enum vect_cost_for_stmt,
-				       class _stmt_vec_info *, int,
+				       class _stmt_vec_info *, tree, int,
 				       enum vect_cost_model_location);
 extern void default_finish_cost (void *, unsigned *, unsigned *, unsigned *);
 extern void default_destroy_cost_data (void *);
@@ -186,6 +186,7 @@ extern tree default_emutls_var_init (tree, tree, tree);
 extern unsigned int default_hard_regno_nregs (unsigned int, machine_mode);
 extern bool default_hard_regno_scratch_ok (unsigned int);
 extern bool default_mode_dependent_address_p (const_rtx, addr_space_t);
+extern bool default_new_address_profitable_p (rtx, rtx_insn *, rtx);
 extern bool default_target_option_valid_attribute_p (tree, tree, tree, int);
 extern bool default_target_option_pragma_parse (tree, tree);
 extern bool default_target_can_inline_p (tree, tree);

'''

EXPECTED2 = '''\
gcc/ChangeLog:

	* targhooks.h (default_add_stmt_cost):
	(default_new_address_profitable_p):

'''

PATCH3 = '''\
diff --git a/libcpp/include/cpplib.h b/libcpp/include/cpplib.h
index 2b1e33f94ae..7f47402f9b9 100644
--- a/libcpp/include/cpplib.h
+++ b/libcpp/include/cpplib.h
@@ -173,7 +173,7 @@ enum c_lang {CLK_GNUC89 = 0, CLK_GNUC99, CLK_GNUC11, CLK_GNUC17, CLK_GNUC2X,
 	     CLK_STDC2X,
 	     CLK_GNUCXX, CLK_CXX98, CLK_GNUCXX11, CLK_CXX11,
 	     CLK_GNUCXX14, CLK_CXX14, CLK_GNUCXX17, CLK_CXX17,
-	     CLK_GNUCXX2A, CLK_CXX2A, CLK_ASM};
+	     CLK_GNUCXX20, CLK_CXX20, CLK_ASM};
 
 /* Payload of a NUMBER, STRING, CHAR or COMMENT token.  */
 struct GTY(()) cpp_string {
@@ -484,7 +484,7 @@ struct cpp_options
   /* Nonzero for C2X decimal floating-point constants.  */
   unsigned char dfp_constants;
 
-  /* Nonzero for C++2a __VA_OPT__ feature.  */
+  /* Nonzero for C++20 __VA_OPT__ feature.  */
   unsigned char va_opt;
 
   /* Nonzero for the '::' token.  */

'''

EXPECTED3 = '''\
libcpp/ChangeLog:

	* include/cpplib.h (enum c_lang):
	(struct cpp_options):

'''

EXPECTED3B = '''\
libcpp/ChangeLog:

	* include/cpplib.h:

'''

PATCH4 = '''\
diff --git a/gcc/ipa-icf.c b/gcc/ipa-icf.c
index aab79492357..f0df1002488 100644
--- a/gcc/ipa-icf.c
+++ b/gcc/ipa-icf.c
@@ -1,5 +1,7 @@
 
 
+
+
 /* Interprocedural Identical Code Folding pass
    Copyright (C) 2014-2020 Free Software Foundation, Inc.
 
diff --git a/gcc/testsuite/gcc.dg/pr32374.c b/gcc/testsuite/gcc.dg/pr32374.c
deleted file mode 100644
index de15d559f5b..00000000000
--- a/gcc/testsuite/gcc.dg/pr32374.c
+++ /dev/null
@@ -1,20 +0,0 @@
-/* { dg-do compile } */
-/* { dg-options "-O2" } */
-
-extern int *stderr;
-
-void f (int *, const char *, ...);
-
-void g (const char *conf_name)
-{
-  typedef struct
-  {
-    const char *label;
-    const int value;
-  } Section;
-
-  const Section sections[2] = { {"", 0}, {"", 1} };
-
-  f (stderr, "", "", conf_name, 0, sections[0]);
-  f (stderr, "", "", conf_name, 0, sections[0]);
-}
diff --git a/gcc/testsuite/gcc.dg/pr40209.c b/gcc/testsuite/gcc.dg/pr40209.c
index 4e77df5c2e6..c23d69d1f1b 100644
--- a/gcc/testsuite/gcc.dg/pr40209.c
+++ b/gcc/testsuite/gcc.dg/pr40209.c
@@ -1,6 +1,8 @@
 /* { dg-do compile } */
 /* { dg-options "-O2 -fprofile-use -fopt-info -Wno-missing-profile" } */
 
+
+
 void process(const char *s);
 
 struct BaseHolder {
diff --git a/gcc/testsuite/gcc.dg/pr50209.c b/gcc/testsuite/gcc.dg/pr50209.c
new file mode 100644
index 00000000000..b28b04f6431
--- /dev/null
+++ b/gcc/testsuite/gcc.dg/pr50209.c
@@ -0,0 +1,3 @@
+
+
+
diff --git a/gcc/testsuite/gcc.dg/pr63567-1.c b/gcc/testsuite/gcc.dg/pr63567-1.c
index 97da171563e..00c5ecc11fa 100644
--- a/gcc/testsuite/gcc.dg/pr63567-1.c
+++ b/gcc/testsuite/gcc.dg/pr63567-1.c
@@ -1,3 +1,4 @@
+
 /* PR c/63567 */
 /* { dg-do compile } */
 /* { dg-options "" } */
diff --git a/gcc/varasm.c b/gcc/varasm.c
index f062e48071f..fd3c7ca8cf3 100644
--- a/gcc/varasm.c
+++ b/gcc/varasm.c
@@ -1,3 +1,5 @@
+
+
 /* Output variables, constants and external declarations, for GNU compiler.
    Copyright (C) 1987-2020 Free Software Foundation, Inc.
 
diff --git a/libssp/gets-chk.c b/libssp/gets-chk.c
index 4ad78c1f77b..6687b368038 100644
--- a/libssp/gets-chk.c
+++ b/libssp/gets-chk.c
@@ -32,6 +32,8 @@ see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 <http://www.gnu.org/licenses/>.  */
 
 
+
+
 #include "config.h"
 #include <ssp/ssp.h>
 #include <stdarg.h>
'''

EXPECTED4 = '''\

	PR 50209

gcc/ChangeLog:

	* ipa-icf.c:
	* varasm.c:

libssp/ChangeLog:

	* gets-chk.c:

gcc/testsuite/ChangeLog:

	* gcc.dg/pr40209.c:
	* gcc.dg/pr63567-1.c:
	* gcc.dg/pr32374.c: Removed.
	* gcc.dg/pr50209.c: New test.

'''

PATCH5 = '''\
diff --git a/gcc/testsuite/gcc.target/i386/pr95046-6.c b/gcc/testsuite/gcc.target/i386/pr95046-6.c
new file mode 100644
index 00000000000..dcc8999c446
--- /dev/null
+++ b/gcc/testsuite/gcc.target/i386/pr95046-6.c
@@ -0,0 +1,44 @@
+/* PR target/95046 */
+/* { dg-do compile { target { ! ia32 } } } */
+/* { dg-options "-O3 -mavx512vl" } */
+
+
+double r[2];
+int s[2];
+unsigned int u[2];
+
+void
+test_float (void)
+{
+  for (int i = 0; i < 2; i++)
+    r[i] = s[i];
+}
+
+/* { dg-final { scan-assembler "\tvcvtdq2pd" } } */
+
+void
+test_ufloat (void)
+{
+  for (int i = 0; i < 2; i++)
+    r[i] = u[i];
+}
+
+/* { dg-final { scan-assembler "\tvcvtudq2pd" } } */
+
+void
+test_fix (void)
+{
+  for (int i = 0; i < 2; i++)
+    s[i] = r[i];
+}
+
+/* { dg-final { scan-assembler "\tvcvttpd2dqx" } } */
+
+void
+test_ufix (void)
+{
+  for (int i = 0; i < 2; i++)
+    u[i] = r[i];
+}
+
+/* { dg-final { scan-assembler "\tvcvttpd2udqx" } } */
-- 
2.26.2

'''

EXPECTED5 = '''\
PR target/95046 - Vectorize V2SFmode operations

	PR target/95046

gcc/testsuite/ChangeLog:

	* gcc.target/i386/pr95046-6.c: New test.

'''

PATCH6 = '''\
diff --git a/gcc/cgraph.h b/gcc/cgraph.h
index 5ddeb65269b..cfae6e91da9 100644
--- a/gcc/cgraph.h
+++ b/gcc/cgraph.h
@@ -937,7 +937,8 @@ struct GTY((tag ("SYMTAB_FUNCTION"))) cgraph_node : public symtab_node
       split_part (false), indirect_call_target (false), local (false),
       versionable (false), can_change_signature (false),
       redefined_extern_inline (false), tm_may_enter_irr (false),
-      ipcp_clone (false), m_uid (uid), m_summary_id (-1)
+      ipcp_clone (false), declare_variant_alt (false),
+      calls_declare_variant_alt (false), m_uid (uid), m_summary_id (-1)
   {}
 
   /* Remove the node from cgraph and all inline clones inlined into it.

'''

EXPECTED6 = '''\
gcc/ChangeLog:

	* cgraph.h (struct cgraph_node):

'''

PATCH7 = '''\
diff --git a/gcc/testsuite/g++.dg/DRs/dr2237.C b/gcc/testsuite/g++.dg/DRs/dr2237.C
new file mode 100644
index 00000000000..f3d6d11e61e
--- /dev/null
+++ b/gcc/testsuite/g++.dg/DRs/dr2237.C
@@ -0,0 +1,18 @@
+// DR 2237 - Can a template-id name a constructor?
+
+template<class T>
+struct X {
+  X<T>(); // { dg-error "expected" "" { target c++20 } }
+  X(int); // OK, injected-class-name used
+  ~X<T>(); // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
+};
+
+// ill-formed since DR1435
+template<typename T> X<T>::X<T>() {} // { dg-error "names the constructor|as no template constructors" }
+template<typename T> X<T>::~X<T>() {} // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
+
+struct Q {
+  // ill-formed since DR1435
+  template<typename T> friend X<T>::X<T>(); // { dg-error "names the constructor|as no template constructors" }
+  template<typename T> friend X<T>::~X<T>(); // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
+};
'''

EXPECTED7 = '''\

	DR 2237

gcc/testsuite/ChangeLog:

	* g++.dg/DRs/dr2237.C: New test.

'''

PATCH8 = '''\
diff --git a/gcc/ipa-icf.c b/gcc/ipa-icf2.c
similarity index 100%
rename from gcc/ipa-icf.c
rename to gcc/ipa-icf2.c
'''

EXPECTED8 = '''\
gcc/ChangeLog:

	* ipa-icf.c: Moved to...
	* ipa-icf2.c: ...here.

'''

PATCH9 = '''\
diff --git a/gcc/config/i386/sse.md b/gcc/config/i386/sse.md
index 2a260c1cfbd..7f03fc491c3 100644
--- a/gcc/config/i386/sse.md
+++ b/gcc/config/i386/sse.md
@@ -17611,6 +17611,23 @@ (define_insn "avx2_<code>v16qiv16hi2<mask_name>"
    (set_attr "prefix" "maybe_evex")
    (set_attr "mode" "OI")])
 
+(define_insn_and_split "*avx2_zero_extendv16qiv16hi2_1"
+  [(set (match_operand:V32QI 0 "register_operand" "=v")
+	(vec_select:V32QI
+	  (vec_concat:V64QI
+	    (match_operand:V32QI 1 "nonimmediate_operand" "vm")
+	    (match_operand:V32QI 2 "const0_operand" "C"))
+	  (match_parallel 3 "pmovzx_parallel"
+	    [(match_operand 4 "const_int_operand" "n")])))]
+  "TARGET_AVX2"
+  "#"
+  "&& reload_completed"
+  [(set (match_dup 0) (zero_extend:V16HI (match_dup 1)))]
+{
+  operands[0] = lowpart_subreg (V16HImode, operands[0], V32QImode);
+  operands[1] = lowpart_subreg (V16QImode, operands[1], V32QImode);
+})
+
 (define_expand "<insn>v16qiv16hi2"
   [(set (match_operand:V16HI 0 "register_operand")
 	(any_extend:V16HI
'''

EXPECTED9 = '''\
gcc/ChangeLog:

	* config/i386/sse.md (*avx2_zero_extendv16qiv16hi2_1):

'''

PATCH10 = '''\
diff --git a/libgomp/doc/the-libgomp-abi/implementing-firstprivate-lastprivate-copyin-and-copyprivate-clauses.rst b/libgomp/doc/the-libgomp-abi/implementing-firstprivate-lastprivate-copyin-and-copyprivate-clauses.rst
new file mode 100644
index 00000000000..ad3c6d856fc
--- /dev/null
+++ b/libgomp/doc/the-libgomp-abi/implementing-firstprivate-lastprivate-copyin-and-copyprivate-clauses.rst
@@ -0,0 +1,3 @@
+
+
+

'''

EXPECTED10 = '''\
libgomp/ChangeLog:

	* doc/the-libgomp-abi/implementing-firstprivate-lastprivate-copyin-and-copyprivate-clauses.rst:
	New file.

'''

class TestMklog(unittest.TestCase):
    def test_macro_definition(self):
        changelog = generate_changelog(PATCH1)
        assert changelog == EXPECTED1

    def test_changed_argument(self):
        changelog = generate_changelog(PATCH2)
        assert changelog == EXPECTED2

    def test_enum_and_struct(self):
        changelog = generate_changelog(PATCH3)
        assert changelog == EXPECTED3

    def test_no_function(self):
        changelog = generate_changelog(PATCH3, True)
        assert changelog == EXPECTED3B

    def test_sorting(self):
        changelog = generate_changelog(PATCH4)
        assert changelog == EXPECTED4

    def test_pr_bugzilla_download(self):
        changelog = generate_changelog(PATCH5, fill_pr_titles=True)
        assert changelog == EXPECTED5

    def test_gty_in_struct(self):
        changelog = generate_changelog(PATCH6, fill_pr_titles=True)
        assert changelog == EXPECTED6

    def test_dr_detection_in_test_case(self):
        changelog = generate_changelog(PATCH7)
        assert changelog == EXPECTED7

    @unittest.skipIf(not unidiff_supports_renaming,
                     'Newer version of unidiff is needed (0.6.0+)')
    def test_renaming(self):
        changelog = generate_changelog(PATCH8)
        assert changelog == EXPECTED8

    def test_define_macro_parsing(self):
        changelog = generate_changelog(PATCH9)
        assert changelog == EXPECTED9

    def test_long_filenames(self):
        changelog = generate_changelog(PATCH10)
        assert changelog == EXPECTED10
