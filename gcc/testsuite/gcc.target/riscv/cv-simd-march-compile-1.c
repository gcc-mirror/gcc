/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */


int
foo1 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_h(a, b, 0);
}


int
foo2 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_b(a, b);
}


int
foo3 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_sc_h(a, b);
}


int
foo4 (int a)
{
	return __builtin_riscv_cv_simd_add_sc_h(a, 20);
}


int
foo5 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_sc_b(a, b);
}


int
foo6 (int a)
{
	return __builtin_riscv_cv_simd_add_sc_b(a, 20);
}


int
foo7 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_h(a, b, 0);
}


int
foo8 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_b(a, b);
}


int
foo9 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_sc_h(a, b);
}


int
foo10 (int a)
{
	return __builtin_riscv_cv_simd_sub_sc_h(a, 20);
}


int
foo11 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_sc_b(a, b);
}


int
foo12 (int a)
{
	return __builtin_riscv_cv_simd_sub_sc_b(a, 20);
}


int
foo13 (int a, int b)
{
	return __builtin_riscv_cv_simd_avg_h(a, b);
}


int
foo14 (int a, int b)
{
	return __builtin_riscv_cv_simd_avg_b(a, b);
}


int
foo15 (int a, int b)
{
	return __builtin_riscv_cv_simd_avg_sc_h(a, b);
}


int
foo16 (int a)
{
	return __builtin_riscv_cv_simd_avg_sc_h(a, 20);
}


int
foo17 (int a, int b)
{
	return __builtin_riscv_cv_simd_avg_sc_b(a, b);
}


int
foo18 (int a)
{
	return __builtin_riscv_cv_simd_avg_sc_b(a, 20);
}


int
foo19 (int a, int b)
{
	return __builtin_riscv_cv_simd_avgu_h(a, b);
}


int
foo20 (int a, int b)
{
	return __builtin_riscv_cv_simd_avgu_b(a, b);
}


int
foo21 (int a, int b)
{
	return __builtin_riscv_cv_simd_avgu_sc_h(a, b);
}


int
foo22 (int a)
{
	return __builtin_riscv_cv_simd_avgu_sc_h(a, 20);
}


int
foo23 (int a, int b)
{
	return __builtin_riscv_cv_simd_avgu_sc_b(a, b);
}


int
foo24 (int a)
{
	return __builtin_riscv_cv_simd_avgu_sc_b(a, 20);
}


int
foo25 (int a, int b)
{
	return __builtin_riscv_cv_simd_min_h(a, b);
}


int
foo26 (int a, int b)
{
	return __builtin_riscv_cv_simd_min_b(a, b);
}


int
foo27 (int a, int b)
{
	return __builtin_riscv_cv_simd_min_sc_h(a, b);
}


int
foo28 (int a)
{
	return __builtin_riscv_cv_simd_min_sc_h(a, 20);
}


int
foo29 (int a, int b)
{
	return __builtin_riscv_cv_simd_min_sc_b(a, b);
}


int
foo30 (int a)
{
	return __builtin_riscv_cv_simd_min_sc_b(a, 20);
}


int
foo31 (int a, int b)
{
	return __builtin_riscv_cv_simd_minu_h(a, b);
}


int
foo32 (int a, int b)
{
	return __builtin_riscv_cv_simd_minu_b(a, b);
}


int
foo33 (int a, int b)
{
	return __builtin_riscv_cv_simd_minu_sc_h(a, b);
}


int
foo34 (int a)
{
	return __builtin_riscv_cv_simd_minu_sc_h(a, 20);
}


int
foo35 (int a, int b)
{
	return __builtin_riscv_cv_simd_minu_sc_b(a, b);
}


int
foo36 (int a)
{
	return __builtin_riscv_cv_simd_minu_sc_b(a, 20);
}


int
foo37 (int a, int b)
{
	return __builtin_riscv_cv_simd_max_h(a, b);
}


int
foo38 (int a, int b)
{
	return __builtin_riscv_cv_simd_max_b(a, b);
}


int
foo39 (int a, int b)
{
	return __builtin_riscv_cv_simd_max_sc_h(a, b);
}


int
foo40 (int a)
{
	return __builtin_riscv_cv_simd_max_sc_h(a, 20);
}


int
foo41 (int a, int b)
{
	return __builtin_riscv_cv_simd_max_sc_b(a, b);
}


int
foo42 (int a)
{
	return __builtin_riscv_cv_simd_max_sc_b(a, 20);
}


int
foo43 (int a, int b)
{
	return __builtin_riscv_cv_simd_maxu_h(a, b);
}


int
foo44 (int a, int b)
{
	return __builtin_riscv_cv_simd_maxu_b(a, b);
}


int
foo45 (int a, int b)
{
	return __builtin_riscv_cv_simd_maxu_sc_h(a, b);
}


int
foo46 (int a)
{
	return __builtin_riscv_cv_simd_maxu_sc_h(a, 20);
}


int
foo47 (int a, int b)
{
	return __builtin_riscv_cv_simd_maxu_sc_b(a, b);
}


int
foo48 (int a)
{
	return __builtin_riscv_cv_simd_maxu_sc_b(a, 20);
}


int
foo49 (int a, int b)
{
	return __builtin_riscv_cv_simd_srl_h(a, b);
}


int
foo50 (int a, int b)
{
	return __builtin_riscv_cv_simd_srl_b(a, b);
}


int
foo51 (int a, int b)
{
	return __builtin_riscv_cv_simd_srl_sc_h(a, b);
}


int
foo52 (int a)
{
	return __builtin_riscv_cv_simd_srl_sc_h(a, 20);
}


int
foo53 (int a, int b)
{
	return __builtin_riscv_cv_simd_srl_sc_b(a, b);
}


int
foo54 (int a)
{
	return __builtin_riscv_cv_simd_srl_sc_b(a, 20);
}


int
foo55 (int a, int b)
{
	return __builtin_riscv_cv_simd_sra_h(a, b);
}


int
foo56 (int a, int b)
{
	return __builtin_riscv_cv_simd_sra_b(a, b);
}


int
foo57 (int a, int b)
{
	return __builtin_riscv_cv_simd_sra_sc_h(a, b);
}


int
foo58 (int a)
{
	return __builtin_riscv_cv_simd_sra_sc_h(a, 20);
}


int
foo59 (int a, int b)
{
	return __builtin_riscv_cv_simd_sra_sc_b(a, b);
}


int
foo60 (int a)
{
	return __builtin_riscv_cv_simd_sra_sc_b(a, 20);
}


int
foo61 (int a, int b)
{
	return __builtin_riscv_cv_simd_sll_h(a, b);
}


int
foo62 (int a, int b)
{
	return __builtin_riscv_cv_simd_sll_b(a, b);
}


int
foo63 (int a, int b)
{
	return __builtin_riscv_cv_simd_sll_sc_h(a, b);
}


int
foo64 (int a)
{
	return __builtin_riscv_cv_simd_sll_sc_h(a, 20);
}


int
foo65 (int a, int b)
{
	return __builtin_riscv_cv_simd_sll_sc_b(a, b);
}


int
foo66 (int a)
{
	return __builtin_riscv_cv_simd_sll_sc_b(a, 20);
}


int
foo67 (int a, int b)
{
	return __builtin_riscv_cv_simd_or_h(a, b);
}


int
foo68 (int a, int b)
{
	return __builtin_riscv_cv_simd_or_b(a, b);
}


int
foo69 (int a, int b)
{
	return __builtin_riscv_cv_simd_or_sc_h(a, b);
}


int
foo70 (int a)
{
	return __builtin_riscv_cv_simd_or_sc_h(a, 20);
}


int
foo71 (int a, int b)
{
	return __builtin_riscv_cv_simd_or_sc_b(a, b);
}


int
foo72 (int a)
{
	return __builtin_riscv_cv_simd_or_sc_b(a, 20);
}


int
foo73 (int a, int b)
{
	return __builtin_riscv_cv_simd_xor_h(a, b);
}


int
foo74 (int a, int b)
{
	return __builtin_riscv_cv_simd_xor_b(a, b);
}


int
foo75 (int a, int b)
{
	return __builtin_riscv_cv_simd_xor_sc_h(a, b);
}


int
foo76 (int a)
{
	return __builtin_riscv_cv_simd_xor_sc_h(a, 20);
}


int
foo77 (int a, int b)
{
	return __builtin_riscv_cv_simd_xor_sc_b(a, b);
}


int
foo78 (int a)
{
	return __builtin_riscv_cv_simd_xor_sc_b(a, 20);
}


int
foo79 (int a, int b)
{
	return __builtin_riscv_cv_simd_and_h(a, b);
}


int
foo80 (int a, int b)
{
	return __builtin_riscv_cv_simd_and_b(a, b);
}


int
foo81 (int a, int b)
{
	return __builtin_riscv_cv_simd_and_sc_h(a, b);
}


int
foo82 (int a)
{
	return __builtin_riscv_cv_simd_and_sc_h(a, 20);
}


int
foo83 (int a, int b)
{
	return __builtin_riscv_cv_simd_and_sc_b(a, b);
}


int
foo84 (int a)
{
	return __builtin_riscv_cv_simd_and_sc_b(a, 20);
}


int
foo85 (int a)
{
	return __builtin_riscv_cv_simd_abs_h(a);
}


int
foo86 (int a)
{
	return __builtin_riscv_cv_simd_abs_b(a);
}


int
foo87 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotup_h(a, b);
}


int
foo88 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotup_b(a, b);
}


int
foo89 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotup_sc_h(a, b);
}


int
foo90 (int a)
{
	return __builtin_riscv_cv_simd_dotup_sc_h(a, 20);
}


int
foo91 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotup_sc_b(a, b);
}


int
foo92 (int a)
{
	return __builtin_riscv_cv_simd_dotup_sc_b(a, 20);
}


int
foo93 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotusp_h(a, b);
}


int
foo94 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotusp_b(a, b);
}


int
foo95 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotusp_sc_h(a, b);
}


int
foo96 (int a)
{
	return __builtin_riscv_cv_simd_dotusp_sc_h(a, 20);
}


int
foo97 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotusp_sc_b(a, b);
}


int
foo98 (int a)
{
	return __builtin_riscv_cv_simd_dotusp_sc_b(a, 20);
}


int
foo99 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotsp_h(a, b);
}


int
foo100 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotsp_b(a, b);
}


int
foo101 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotsp_sc_h(a, b);
}


int
foo102 (int a)
{
	return __builtin_riscv_cv_simd_dotsp_sc_h(a, 20);
}


int
foo103 (int a, int b)
{
	return __builtin_riscv_cv_simd_dotsp_sc_b(a, b);
}


int
foo104 (int a)
{
	return __builtin_riscv_cv_simd_dotsp_sc_b(a, 20);
}


int
foo105 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotup_h(a, b, c);
}


int
foo106 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotup_b(a, b, c);
}


int
foo107 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotup_sc_h(a, b, c);
}


int
foo108 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotup_sc_h(a, 20, b);
}


int
foo109 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotup_sc_b(a, b, c);
}


int
foo110 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotup_sc_b(a, 20, b);
}


int
foo111 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotusp_h(a, b, c);
}


int
foo112 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotusp_b(a, b, c);
}


int
foo113 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotusp_sc_h(a, b, c);
}


int
foo114 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotusp_sc_h(a, 20, b);
}


int
foo115 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotusp_sc_b(a, b, c);
}


int
foo116 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotusp_sc_b(a, 20, b);
}


int
foo117 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotsp_h(a, b, c);
}


int
foo118 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotsp_b(a, b, c);
}


int
foo119 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotsp_sc_h(a, b, c);
}


int
foo120 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotsp_sc_h(a, 20, b);
}


int
foo121 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotsp_sc_b(a, b, c);
}


int
foo122 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotsp_sc_b(a, 20, b);
}


int
foo123 (int a)
{
	return __builtin_riscv_cv_simd_extract_h(a, 20);
}


int
foo124 (int a)
{
	return __builtin_riscv_cv_simd_extract_b(a, 20);
}


int
foo125 (int a)
{
	return __builtin_riscv_cv_simd_extractu_h(a, 20);
}


int
foo126 (int a)
{
	return __builtin_riscv_cv_simd_extractu_b(a, 20);
}


int
foo127 (int a, int b)
{
	return __builtin_riscv_cv_simd_insert_h(a, b, 20);
}


int
foo128 (int a, int b)
{
	return __builtin_riscv_cv_simd_insert_b(a, b, 20);
}


int
foo129 (int a, int b)
{
	return __builtin_riscv_cv_simd_shuffle_h(a, b);
}


int
foo130 (int a, int b)
{
	return __builtin_riscv_cv_simd_shuffle_b(a, b);
}


int
foo131 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_h(a, 20);
}


int
foo132 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_b(a, 0);
}


int
foo133 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_b(a, 64);
}


int
foo134 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_b(a, 128);
}


int
foo135 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_b(a, 192);
}


int
foo136 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_shuffle2_h(a, b, c);
}


int
foo137 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_shuffle2_b(a, b, c);
}


int
foo138 (int a, int b)
{
	return __builtin_riscv_cv_simd_packlo_h(a, b);
}


int
foo139 (int a, int b)
{
	return __builtin_riscv_cv_simd_packhi_h(a, b);
}


int
foo140 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_packhi_b(a, b, c);
}


int
foo141 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_packlo_b(a, b, c);
}


int
foo142 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpeq_h(a, b);
}


int
foo143 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpeq_b(a, b);
}


int
foo144 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_h(a, b);
}


int
foo145 (int a)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_h(a, 20);
}


int
foo146 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, b);
}


int
foo147 (int a)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, 20);
}


int
foo148 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpne_h(a, b);
}


int
foo149 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpne_b(a, b);
}


int
foo150 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpne_sc_h(a, b);
}


int
foo151 (int a)
{
	return __builtin_riscv_cv_simd_cmpne_sc_h(a, 20);
}


int
foo152 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpne_sc_b(a, b);
}


int
foo153 (int a)
{
	return __builtin_riscv_cv_simd_cmpne_sc_b(a, 20);
}


int
foo154 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgt_h(a, b);
}


int
foo155 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgt_b(a, b);
}


int
foo156 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgt_sc_h(a, b);
}


int
foo157 (int a)
{
	return __builtin_riscv_cv_simd_cmpgt_sc_h(a, 20);
}


int
foo158 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgt_sc_b(a, b);
}


int
foo159 (int a)
{
	return __builtin_riscv_cv_simd_cmpgt_sc_b(a, 20);
}


int
foo160 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpge_h(a, b);
}


int
foo161 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpge_b(a, b);
}


int
foo162 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpge_sc_h(a, b);
}


int
foo163 (int a)
{
	return __builtin_riscv_cv_simd_cmpge_sc_h(a, 20);
}


int
foo164 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpge_sc_b(a, b);
}


int
foo165 (int a)
{
	return __builtin_riscv_cv_simd_cmpge_sc_b(a, 20);
}


int
foo166 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmplt_h(a, b);
}


int
foo167 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmplt_b(a, b);
}


int
foo168 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmplt_sc_h(a, b);
}


int
foo169 (int a)
{
	return __builtin_riscv_cv_simd_cmplt_sc_h(a, 20);
}


int
foo170 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmplt_sc_b(a, b);
}


int
foo171 (int a)
{
	return __builtin_riscv_cv_simd_cmplt_sc_b(a, 20);
}


int
foo172 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmple_h(a, b);
}


int
foo173 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmple_b(a, b);
}


int
foo174 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmple_sc_h(a, b);
}


int
foo175 (int a)
{
	return __builtin_riscv_cv_simd_cmple_sc_h(a, 20);
}


int
foo176 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmple_sc_b(a, b);
}


int
foo177 (int a)
{
	return __builtin_riscv_cv_simd_cmple_sc_b(a, 20);
}


int
foo178 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgtu_h(a, b);
}


int
foo179 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgtu_b(a, b);
}


int
foo180 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgtu_sc_h(a, b);
}


int
foo181 (int a)
{
	return __builtin_riscv_cv_simd_cmpgtu_sc_h(a, 20);
}


int
foo182 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgtu_sc_b(a, b);
}


int
foo183 (int a)
{
	return __builtin_riscv_cv_simd_cmpgtu_sc_b(a, 20);
}


int
foo184 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgeu_h(a, b);
}


int
foo185 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgeu_b(a, b);
}


int
foo186 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgeu_sc_h(a, b);
}


int
foo187 (int a)
{
	return __builtin_riscv_cv_simd_cmpgeu_sc_h(a, 20);
}


int
foo188 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpgeu_sc_b(a, b);
}


int
foo189 (int a)
{
	return __builtin_riscv_cv_simd_cmpgeu_sc_b(a, 20);
}


int
foo190 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpltu_h(a, b);
}


int
foo191 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpltu_b(a, b);
}


int
foo192 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpltu_sc_h(a, b);
}


int
foo193 (int a)
{
	return __builtin_riscv_cv_simd_cmpltu_sc_h(a, 20);
}


int
foo194 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpltu_sc_b(a, b);
}


int
foo195 (int a)
{
	return __builtin_riscv_cv_simd_cmpltu_sc_b(a, 20);
}


int
foo196 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpleu_h(a, b);
}


int
foo197 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpleu_b(a, b);
}


int
foo198 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpleu_sc_h(a, b);
}


int
foo199 (int a)
{
	return __builtin_riscv_cv_simd_cmpleu_sc_h(a, 20);
}


int
foo200 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpleu_sc_b(a, b);
}


int
foo201 (int a)
{
	return __builtin_riscv_cv_simd_cmpleu_sc_b(a, 20);
}


int
foo202 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_r(a, b, c, 0);
}


int
foo203 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_i(a, b, c, 0);
}


int
foo204 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_r(a, b, c, 1);
}


int
foo205 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_i(a, b, c, 1);
}


int
foo206 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_r(a, b, c, 2);
}


int
foo207 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_i(a, b, c, 2);
}


int
foo208 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_r(a, b, c, 3);
}


int
foo209 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_i(a, b, c, 3);
}


int
foo210 (int a)
{
	return __builtin_riscv_cv_simd_cplxconj(a);
}


int
foo211 (int a, int b)
{
	return __builtin_riscv_cv_simd_subrotmj(a, b, 0);
}


int
foo212 (int a, int b)
{
	return __builtin_riscv_cv_simd_subrotmj(a, b, 1);
}


int
foo213 (int a, int b)
{
	return __builtin_riscv_cv_simd_subrotmj(a, b, 2);
}


int
foo214 (int a, int b)
{
	return __builtin_riscv_cv_simd_subrotmj(a, b, 3);
}


int
foo215 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_h(a, b, 1);
}


int
foo216 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_h(a, b, 2);
}


int
foo217 (int a, int b)
{
	return __builtin_riscv_cv_simd_add_h(a, b, 3);
}


int
foo218 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_h(a, b, 1);
}


int
foo219 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_h(a, b, 2);
}


int
foo220 (int a, int b)
{
	return __builtin_riscv_cv_simd_sub_h(a, b, 3);
}


/* { dg-final { scan-assembler-times "cv\\.add\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avg\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.avgu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.max\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.srl\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sra\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sll\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.or\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.xor\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.and\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.abs\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.abs\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotup\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotusp\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.dotsp\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotusp\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotsp\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.extract\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.extract\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.extractu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.extractu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.insert\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.insert\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shuffle\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shuffle\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shuffle\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei0\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei1\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei2\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei3\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shuffle2\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shuffle2\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.pack" 4 } } */
/* { dg-final { scan-assembler-times "cv\\.pack\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.packhi\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.packlo\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpne\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgt\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpge\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmplt\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmple\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgtu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpgeu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpltu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.sc\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.sci\\.h" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpleu\\.sci\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.r" 4 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.i" 4 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.r\\.div2" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.i\\.div2" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.r\\.div4" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.i\\.div4" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.r\\.div8" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.i\\.div8" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cplxconj" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subrotmj" 4 } } */
/* { dg-final { scan-assembler-times "cv\\.subrotmj\\.div2" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subrotmj\\.div4" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subrotmj\\.div8" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.div2" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.div4" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.add\\.div8" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.div2" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.div4" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sub\\.div8" 1 } } */
