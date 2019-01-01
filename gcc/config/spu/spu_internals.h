/* Definitions of Synergistic Processing Unit (SPU). */
/* Copyright (C) 2006-2019 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef  _SPU_INTERNALS_H
#define _SPU_INTERNALS_H 
 
/* For a typical GCC implementation, the vector keyword is defined here
 * as a macro.  If this macro conflicts with user code the user needs to
 * undefine it.  An extended GCC implementation may implement this
 * keyword differently, such that it never conflicts,  and will define
 * the macro __VECTOR_KEYWORD_SUPPORTED__. */
#ifndef __VECTOR_KEYWORD_SUPPORTED__
#define vector __vector
#endif


/*  The spu specific instruction macros, si_*(), correspond 1-1 with
 *  SPU instructions in the ISA.  The arguments are the same with the
 *  following exceptions:
 *   -  any instruction which both reads and writes rt will have an
 *      extra parameter in the macro.
 *   -  instructions which append zero to the immediate field assume
 *      the value given in a macro already has the zeroes appended.
 *   -  integer/float convert functions expect a value from 0 to 127,
 *      i.e., the bias is added by the compiler.
 *
 *  Parameters named 'imm' accept an integer literal.
 *  Parameters named 'r[abcdt]' accept a qword argument.
 *  Parameters named 'scalar' accept a scalar argument.
 */

#define qword __vector signed char

#define si_lqd(ra,imm)       __builtin_si_lqd(ra,imm)
#define si_lqx(ra,rb)        __builtin_si_lqx(ra,rb)
#define si_lqa(imm)          __builtin_si_lqa(imm)
#define si_lqr(imm)          __builtin_si_lqr(imm)
#define si_stqd(rt,ra,imm)   __builtin_si_stqd(rt,ra,imm)
#define si_stqx(rt,ra,rb)    __builtin_si_stqx(rt,ra,rb)
#define si_stqa(rt,imm)      __builtin_si_stqa(rt,imm)
#define si_stqr(rt,imm)      __builtin_si_stqr(rt,imm)
#define si_cbd(ra,imm)       __builtin_si_cbd(ra,imm)
#define si_cbx(ra,rb)        __builtin_si_cbx(ra,rb)
#define si_chd(ra,imm)       __builtin_si_chd(ra,imm)
#define si_chx(ra,rb)        __builtin_si_chx(ra,rb)
#define si_cwd(ra,imm)       __builtin_si_cwd(ra,imm)
#define si_cwx(ra,rb)        __builtin_si_cwx(ra,rb)
#define si_cdd(ra,imm)       __builtin_si_cdd(ra,imm)
#define si_cdx(ra,rb)        __builtin_si_cdx(ra,rb)
#define si_ilh(imm)          __builtin_si_ilh(imm)
#define si_ilhu(imm)         __builtin_si_ilhu(imm)
#define si_il(imm)           __builtin_si_il(imm)
#define si_ila(imm)          __builtin_si_ila(imm)
#define si_iohl(ra,imm)      __builtin_si_iohl(ra,imm)
#define si_fsmbi(imm)        __builtin_si_fsmbi(imm)
#define si_ah(ra,rb)         __builtin_si_ah(ra,rb)
#define si_ahi(ra,imm)       __builtin_si_ahi(ra,imm)
#define si_a(ra,rb)          __builtin_si_a(ra,rb)
#define si_ai(ra,imm)        __builtin_si_ai(ra,imm)
#define si_addx(ra,rb,rt)    __builtin_si_addx(ra,rb,rt)
#define si_cg(ra,rb)         __builtin_si_cg(ra,rb)
#define si_cgx(ra,rb,rt)     __builtin_si_cgx(ra,rb,rt)
#define si_sfh(ra,rb)        __builtin_si_sfh(ra,rb)
#define si_sfhi(imm,ra)      __builtin_si_sfhi(imm,ra)
#define si_sf(ra,rb)         __builtin_si_sf(ra,rb)
#define si_sfi(ra,imm)       __builtin_si_sfi(ra,imm)
#define si_sfx(ra,rb,rt)     __builtin_si_sfx(ra,rb,rt)
#define si_bg(ra,rb)         __builtin_si_bg(ra,rb)
#define si_bgx(ra,rb,rt)     __builtin_si_bgx(ra,rb,rt)
#define si_mpy(ra,rb)        __builtin_si_mpy(ra,rb)
#define si_mpyu(ra,rb)       __builtin_si_mpyu(ra,rb)
#define si_mpyi(ra,imm)      __builtin_si_mpyi(ra,imm)
#define si_mpyui(ra,imm)     __builtin_si_mpyui(ra,imm)
#define si_mpya(ra,rb,rc)    __builtin_si_mpya(ra,rb,rc)
#define si_mpyh(ra,rb)       __builtin_si_mpyh(ra,rb)
#define si_mpys(ra,rb)       __builtin_si_mpys(ra,rb)
#define si_mpyhh(ra,rb)      __builtin_si_mpyhh(ra,rb)
#define si_mpyhhu(ra,rb)     __builtin_si_mpyhhu(ra,rb)
#define si_mpyhha(ra,rb,rc)  __builtin_si_mpyhha(ra,rb,rc)
#define si_mpyhhau(ra,rb,rc) __builtin_si_mpyhhau(ra,rb,rc)
#define si_clz(ra)           __builtin_si_clz(ra)
#define si_cntb(ra)          __builtin_si_cntb(ra)
#define si_fsmb(ra)          __builtin_si_fsmb(ra)
#define si_fsmh(ra)          __builtin_si_fsmh(ra)
#define si_fsm(ra)           __builtin_si_fsm(ra)
#define si_gbb(ra)           __builtin_si_gbb(ra)
#define si_gbh(ra)           __builtin_si_gbh(ra)
#define si_gb(ra)            __builtin_si_gb(ra)
#define si_avgb(ra,rb)       __builtin_si_avgb(ra,rb)
#define si_absdb(ra,rb)      __builtin_si_absdb(ra,rb)
#define si_sumb(ra,rb)       __builtin_si_sumb(ra,rb)
#define si_xsbh(ra)          __builtin_si_xsbh(ra)
#define si_xshw(ra)          __builtin_si_xshw(ra)
#define si_xswd(ra)          __builtin_si_xswd(ra)
#define si_and(ra,rb)        __builtin_si_and(ra,rb)
#define si_andc(ra,rb)       __builtin_si_andc(ra,rb)
#define si_andbi(ra,imm)     __builtin_si_andbi(ra,imm)
#define si_andhi(ra,imm)     __builtin_si_andhi(ra,imm)
#define si_andi(ra,imm)      __builtin_si_andi(ra,imm)
#define si_or(ra,rb)         __builtin_si_or(ra,rb)
#define si_orc(ra,rb)        __builtin_si_orc(ra,rb)
#define si_orbi(ra,imm)      __builtin_si_orbi(ra,imm)
#define si_orhi(ra,imm)      __builtin_si_orhi(ra,imm)
#define si_ori(ra,imm)       __builtin_si_ori(ra,imm)
#define si_orx(ra)           __builtin_si_orx(ra)
#define si_xor(ra,rb)        __builtin_si_xor(ra,rb)
#define si_xorbi(ra,imm)     __builtin_si_xorbi(ra,imm)
#define si_xorhi(ra,imm)     __builtin_si_xorhi(ra,imm)
#define si_xori(ra,imm)      __builtin_si_xori(ra,imm)
#define si_nand(ra,rb)       __builtin_si_nand(ra,rb)
#define si_nor(ra,rb)        __builtin_si_nor(ra,rb)
#define si_eqv(ra,rb)        __builtin_si_eqv(ra,rb)
#define si_selb(ra,rb,rc)    __builtin_si_selb(ra,rb,rc)
#define si_shufb(ra,rb,rc)   __builtin_si_shufb(ra,rb,rc)
#define si_shlh(ra,rb)       __builtin_si_shlh(ra,rb)
#define si_shlhi(ra,imm)     __builtin_si_shlhi(ra,imm)
#define si_shl(ra,rb)        __builtin_si_shl(ra,rb)
#define si_shli(ra,imm)      __builtin_si_shli(ra,imm)
#define si_shlqbi(ra,rb)     __builtin_si_shlqbi(ra,rb)
#define si_shlqbii(ra,imm)   __builtin_si_shlqbii(ra,imm)
#define si_shlqby(ra,rb)     __builtin_si_shlqby(ra,rb)
#define si_shlqbyi(ra,imm)   __builtin_si_shlqbyi(ra,imm)
#define si_shlqbybi(ra,rb)   __builtin_si_shlqbybi(ra,rb)
#define si_roth(ra,rb)       __builtin_si_roth(ra,rb)
#define si_rothi(ra,imm)     __builtin_si_rothi(ra,imm)
#define si_rot(ra,rb)        __builtin_si_rot(ra,rb)
#define si_roti(ra,imm)      __builtin_si_roti(ra,imm)
#define si_rotqby(ra,rb)     __builtin_si_rotqby(ra,rb)
#define si_rotqbyi(ra,imm)   __builtin_si_rotqbyi(ra,imm)
#define si_rotqbybi(ra,rb)   __builtin_si_rotqbybi(ra,rb)
#define si_rotqbi(ra,rb)     __builtin_si_rotqbi(ra,rb)
#define si_rotqbii(ra,imm)   __builtin_si_rotqbii(ra,imm)
#define si_rothm(ra,rb)      __builtin_si_rothm(ra,rb)
#define si_rothmi(ra,imm)    __builtin_si_rothmi(ra,imm)
#define si_rotm(ra,rb)       __builtin_si_rotm(ra,rb)
#define si_rotmi(ra,imm)     __builtin_si_rotmi(ra,imm)
#define si_rotqmby(ra,rb)    __builtin_si_rotqmby(ra,rb)
#define si_rotqmbyi(ra,imm)  __builtin_si_rotqmbyi(ra,imm)
#define si_rotqmbi(ra,rb)    __builtin_si_rotqmbi(ra,rb)
#define si_rotqmbii(ra,imm)  __builtin_si_rotqmbii(ra,imm)
#define si_rotqmbybi(ra,rb)  __builtin_si_rotqmbybi(ra,rb)
#define si_rotmah(ra,rb)     __builtin_si_rotmah(ra,rb)
#define si_rotmahi(ra,imm)   __builtin_si_rotmahi(ra,imm)
#define si_rotma(ra,rb)      __builtin_si_rotma(ra,rb)
#define si_rotmai(ra,imm)    __builtin_si_rotmai(ra,imm)
#define si_heq(ra,rb)        __builtin_si_heq(ra,rb)
#define si_heqi(ra,imm)      __builtin_si_heqi(ra,imm)
#define si_hgt(ra,rb)        __builtin_si_hgt(ra,rb)
#define si_hgti(ra,imm)      __builtin_si_hgti(ra,imm)
#define si_hlgt(ra,rb)       __builtin_si_hlgt(ra,rb)
#define si_hlgti(ra,imm)     __builtin_si_hlgti(ra,imm)
#define si_ceqb(ra,rb)       __builtin_si_ceqb(ra,rb)
#define si_ceqbi(ra,imm)     __builtin_si_ceqbi(ra,imm)
#define si_ceqh(ra,rb)       __builtin_si_ceqh(ra,rb)
#define si_ceqhi(ra,imm)     __builtin_si_ceqhi(ra,imm)
#define si_ceq(ra,rb)        __builtin_si_ceq(ra,rb)
#define si_ceqi(ra,imm)      __builtin_si_ceqi(ra,imm)
#define si_cgtb(ra,rb)       __builtin_si_cgtb(ra,rb)
#define si_cgtbi(ra,imm)     __builtin_si_cgtbi(ra,imm)
#define si_cgth(ra,rb)       __builtin_si_cgth(ra,rb)
#define si_cgthi(ra,imm)     __builtin_si_cgthi(ra,imm)
#define si_cgt(ra,rb)        __builtin_si_cgt(ra,rb)
#define si_cgti(ra,imm)      __builtin_si_cgti(ra,imm)
#define si_clgtb(ra,rb)      __builtin_si_clgtb(ra,rb)
#define si_clgtbi(ra,imm)    __builtin_si_clgtbi(ra,imm)
#define si_clgth(ra,rb)      __builtin_si_clgth(ra,rb)
#define si_clgthi(ra,imm)    __builtin_si_clgthi(ra,imm)
#define si_clgt(ra,rb)       __builtin_si_clgt(ra,rb)
#define si_clgti(ra,imm)     __builtin_si_clgti(ra,imm)
#define si_bisled(ra)        __builtin_si_bisled(ra,0)
#define si_bisledd(ra)       __builtin_si_bisledd(ra,0)
#define si_bislede(ra)       __builtin_si_bislede(ra,0)
#define si_fa(ra,rb)         __builtin_si_fa(ra,rb)
#define si_dfa(ra,rb)        __builtin_si_dfa(ra,rb)
#define si_fs(ra,rb)         __builtin_si_fs(ra,rb)
#define si_dfs(ra,rb)        __builtin_si_dfs(ra,rb)
#define si_fm(ra,rb)         __builtin_si_fm(ra,rb)
#define si_dfm(ra,rb)        __builtin_si_dfm(ra,rb)
#define si_fma(ra,rb,rc)     __builtin_si_fma(ra,rb,rc)
#define si_dfma(ra,rb,rc)    __builtin_si_dfma(ra,rb,rc)
#define si_dfnma(ra,rb,rc)   __builtin_si_dfnma(ra,rb,rc)
#define si_fnms(ra,rb,rc)    __builtin_si_fnms(ra,rb,rc)
#define si_dfnms(ra,rb,rc)   __builtin_si_dfnms(ra,rb,rc)
#define si_fms(ra,rb,rc)     __builtin_si_fms(ra,rb,rc)
#define si_dfms(ra,rb,rc)    __builtin_si_dfms(ra,rb,rc)
#define si_frest(ra)         __builtin_si_frest(ra)
#define si_frsqest(ra)       __builtin_si_frsqest(ra)
#define si_fi(ra,rb)         __builtin_si_fi(ra,rb)
#define si_csflt(ra,imm)     __builtin_si_csflt(ra,imm)
#define si_cflts(ra,imm)     __builtin_si_cflts(ra,imm)
#define si_cuflt(ra,imm)     __builtin_si_cuflt(ra,imm)
#define si_cfltu(ra,imm)     __builtin_si_cfltu(ra,imm)
#define si_frds(ra)          __builtin_si_frds(ra)
#define si_fesd(ra)          __builtin_si_fesd(ra)
#define si_fceq(ra,rb)       __builtin_si_fceq(ra,rb)
#define si_fcmeq(ra,rb)      __builtin_si_fcmeq(ra,rb)
#define si_fcgt(ra,rb)       __builtin_si_fcgt(ra,rb)
#define si_fcmgt(ra,rb)      __builtin_si_fcmgt(ra,rb)
#define si_stop(imm)         __builtin_si_stop(imm)
#define si_stopd(ra,rb,rc)   __builtin_si_stopd(ra,rb,rc)
#define si_lnop()            __builtin_si_lnop()
#define si_nop()             __builtin_si_nop()
#define si_sync()            __builtin_si_sync()
#define si_syncc()           __builtin_si_syncc()
#define si_dsync()           __builtin_si_dsync()
#define si_mfspr(imm)        __builtin_si_mfspr(imm)
#define si_mtspr(imm,ra)     __builtin_si_mtspr(imm,ra)
#define si_fscrrd()          __builtin_si_fscrrd()
#define si_fscrwr(ra)        __builtin_si_fscrwr(ra)
#define si_rdch(imm)         __builtin_si_rdch(imm)
#define si_rchcnt(imm)       __builtin_si_rchcnt(imm)
#define si_wrch(imm,ra)      __builtin_si_wrch(imm,ra)

/* celledp only instructions  */
#ifdef __SPU_EDP__
#define si_dfceq(ra,rb)      __builtin_si_dfceq(ra,rb)
#define si_dfcmeq(ra,rb)     __builtin_si_dfcmeq(ra,rb)
#define si_dfcgt(ra,rb)      __builtin_si_dfcgt(ra,rb)
#define si_dfcmgt(ra,rb)     __builtin_si_dfcmgt(ra,rb)
#define si_dftsv(ra,imm)     __builtin_si_dftsv(ra,imm)
#endif /* __SPU_EDP__  */

#define si_from_char(scalar)    __builtin_si_from_char(scalar)
#define si_from_uchar(scalar)   __builtin_si_from_uchar(scalar)
#define si_from_short(scalar)   __builtin_si_from_short(scalar)
#define si_from_ushort(scalar)  __builtin_si_from_ushort(scalar)
#define si_from_int(scalar)     __builtin_si_from_int(scalar)
#define si_from_uint(scalar)    __builtin_si_from_uint(scalar)
#define si_from_llong(scalar)   __builtin_si_from_long(scalar)
#define si_from_ullong(scalar)  __builtin_si_from_ulong(scalar)
#define si_from_float(scalar)   __builtin_si_from_float(scalar)
#define si_from_double(scalar)  __builtin_si_from_double(scalar)
#define si_from_ptr(scalar)     __builtin_si_from_ptr(scalar)

#define si_to_char(ra)      __builtin_si_to_char(ra)
#define si_to_uchar(ra)     __builtin_si_to_uchar(ra)
#define si_to_short(ra)     __builtin_si_to_short(ra)
#define si_to_ushort(ra)    __builtin_si_to_ushort(ra)
#define si_to_int(ra)       __builtin_si_to_int(ra)
#define si_to_uint(ra)      __builtin_si_to_uint(ra)
#define si_to_llong(ra)     __builtin_si_to_long(ra)
#define si_to_ullong(ra)    __builtin_si_to_ulong(ra)
#define si_to_float(ra)     __builtin_si_to_float(ra)
#define si_to_double(ra)    __builtin_si_to_double(ra)
#define si_to_ptr(ra)       __builtin_si_to_ptr(ra)

#define __align_hint(ptr,base,offset) __builtin_spu_align_hint(ptr,base,offset)

/* generic spu_* intrinsics */

#define spu_splats(scalar)        __builtin_spu_splats(scalar) 
#define spu_convtf(ra,imm)        __builtin_spu_convtf(ra,imm)
#define spu_convts(ra,imm)        __builtin_spu_convts(ra,imm)
#define spu_convtu(ra,imm)        __builtin_spu_convtu(ra,imm) 
#define spu_extend(ra)            __builtin_spu_extend(ra) 
#define spu_roundtf(ra)           __builtin_spu_roundtf(ra) 
#define spu_add(ra,rb)            __builtin_spu_add(ra,rb) 
#define spu_addx(ra,rb,rt)        __builtin_spu_addx(ra,rb,rt) 
#define spu_genc(ra,rb)           __builtin_spu_genc(ra,rb) 
#define spu_gencx(ra,rb,rt)       __builtin_spu_gencx(ra,rb,rt) 
#define spu_madd(ra,rb,rc)        __builtin_spu_madd(ra,rb,rc)
#define spu_nmadd(ra,rb,rc)       __builtin_spu_nmadd(ra,rb,rc)
#define spu_mhhadd(ra,rb,rc)      __builtin_spu_mhhadd(ra,rb,rc)
#define spu_msub(ra,rb,rc)        __builtin_spu_msub(ra,rb,rc) 
#define spu_mul(ra,rb)            __builtin_spu_mul(ra,rb) 
#define spu_mulh(ra,rb)           __builtin_spu_mulh(ra,rb) 
#define spu_mule(ra,rb)           __builtin_spu_mule(ra,rb) 
#define spu_mulo(ra,rb)           __builtin_spu_mulo(ra,rb) 
#define spu_mulsr(ra,rb)          __builtin_spu_mulsr(ra,rb) 
#define spu_nmsub(ra,rb,rc)       __builtin_spu_nmsub(ra,rb,rc) 
#define spu_sub(ra,rb)            __builtin_spu_sub(ra,rb)
#define spu_subx(ra,rb,rt)        __builtin_spu_subx(ra,rb,rt) 
#define spu_genb(ra,rb)           __builtin_spu_genb(ra,rb) 
#define spu_genbx(ra,rb,rt)       __builtin_spu_genbx(ra,rb,rt) 
#define spu_absd(ra,rb)           __builtin_spu_absd(ra,rb) 
#define spu_avg(ra,rb)            __builtin_spu_avg(ra,rb) 
#define spu_sumb(ra,rb)           __builtin_spu_sumb(ra,rb) 
#define spu_bisled(ra)            __builtin_spu_bisled(ra, 0)
#define spu_bisled_d(ra)          __builtin_spu_bisled_d(ra, 0)
#define spu_bisled_e(ra)          __builtin_spu_bisled_e(ra, 0)
#define spu_cmpabseq(ra,rb)       __builtin_spu_cmpabseq(ra,rb) 
#define spu_cmpabsgt(ra,rb)       __builtin_spu_cmpabsgt(ra,rb) 
#define spu_cmpeq(ra,rb)          __builtin_spu_cmpeq(ra,rb) 
#define spu_cmpgt(ra,rb)          __builtin_spu_cmpgt(ra,rb) 
#define spu_testsv(ra,imm)        __builtin_spu_testsv(ra,imm) 
#define spu_hcmpeq(ra,rb)         __builtin_spu_hcmpeq(ra,rb) 
#define spu_hcmpgt(ra,rb)         __builtin_spu_hcmpgt(ra,rb) 
#define spu_cntb(ra)              __builtin_spu_cntb(ra) 
#define spu_cntlz(ra)             __builtin_spu_cntlz(ra) 
#define spu_gather(ra)            __builtin_spu_gather(ra) 
#define spu_maskb(ra)             __builtin_spu_maskb(ra) 
#define spu_maskh(ra)             __builtin_spu_maskh(ra) 
#define spu_maskw(ra)             __builtin_spu_maskw(ra) 
#define spu_sel(ra,rb,rc)         __builtin_spu_sel(ra,rb,rc) 
#define spu_shuffle(ra,rb,rc)     __builtin_spu_shuffle(ra,rb,rc) 
#define spu_and(ra,rb)            __builtin_spu_and(ra,rb) 
#define spu_andc(ra,rb)           __builtin_spu_andc(ra,rb) 
#define spu_eqv(ra,rb)            __builtin_spu_eqv(ra,rb) 
#define spu_nand(ra,rb)           __builtin_spu_nand(ra,rb)
#define spu_nor(ra,rb)            __builtin_spu_nor(ra,rb) 
#define spu_or(ra,rb)             __builtin_spu_or(ra,rb) 
#define spu_orc(ra,rb)            __builtin_spu_orc(ra,rb) 
#define spu_orx(ra)               __builtin_spu_orx(ra)
#define spu_xor(ra,rb)            __builtin_spu_xor(ra,rb) 
#define spu_rl(ra,rb)             __builtin_spu_rl(ra,rb) 
#define spu_rlqw(ra,count)        __builtin_spu_rlqw(ra,count) 
#define spu_rlqwbyte(ra,count)    __builtin_spu_rlqwbyte(ra,count) 
#define spu_rlqwbytebc(ra,count)  __builtin_spu_rlqwbytebc(ra,count) 
#define spu_rlmask(ra,rb)         __builtin_spu_rlmask(ra,rb) 
#define spu_rlmaska(ra,rb)        __builtin_spu_rlmaska(ra,rb) 
#define spu_rlmaskqw(ra,rb)       __builtin_spu_rlmaskqw(ra,rb) 
#define spu_rlmaskqwbyte(ra,rb)   __builtin_spu_rlmaskqwbyte(ra,rb) 
#define spu_rlmaskqwbytebc(ra,rb) __builtin_spu_rlmaskqwbytebc(ra,rb) 
#define spu_sl(ra,rb)             __builtin_spu_sl(ra,rb) 
#define spu_slqw(ra,rb)           __builtin_spu_slqw(ra,rb) 
#define spu_slqwbyte(ra,rb)       __builtin_spu_slqwbyte(ra,rb) 
#define spu_slqwbytebc(ra,rb)     __builtin_spu_slqwbytebc(ra,rb) 
#define spu_sr(ra,rb)             __builtin_spu_sr(ra,rb) 
#define spu_sra(ra,rb)            __builtin_spu_sra(ra,rb) 
#define spu_srqw(ra,rb)           __builtin_spu_srqw(ra,rb) 
#define spu_srqwbyte(ra,rb)       __builtin_spu_srqwbyte(ra,rb) 
#define spu_srqwbytebc(ra,rb)     __builtin_spu_srqwbytebc(ra,rb) 
#define spu_extract(ra,pos)       __builtin_spu_extract(ra,pos) 
#define spu_insert(scalar,ra,pos) __builtin_spu_insert(scalar,ra,pos) 
#define spu_promote(scalar,pos)   __builtin_spu_promote(scalar,pos) 

#ifdef __cplusplus
extern "C" {
#endif

/* The type checking for some of these won't be accurate but they need
 * to be defines because of the immediate values. */
#define spu_idisable()          __builtin_spu_idisable()
#define spu_ienable()           __builtin_spu_ienable()
#define spu_mfspr(imm)          si_to_uint(si_mfspr((imm)))
#define spu_mtspr(imm, ra)      si_mtspr((imm),si_from_uint (ra))
#define spu_mffpscr()           ((vec_uint4)si_fscrrd())
#define spu_mtfpscr(a)          si_fscrwr((qword)a)
#define spu_dsync()             si_dsync() 
#define spu_stop(imm)           si_stop(imm)
#define spu_sync()              si_sync()
#define spu_sync_c()            si_syncc()
#define spu_readch(imm)         si_to_uint(si_rdch((imm)))
#define spu_readchqw(imm)       ((vec_uint4)si_rdch((imm)))
#define spu_readchcnt(imm)      si_to_uint(si_rchcnt((imm)))
#define spu_writech(imm, ra)    si_wrch((imm), si_from_uint(ra))
#define spu_writechqw(imm, ra)  si_wrch((imm), (qword)(ra))

/* The following functions are static and always_inline to make sure
 * they don't show up in object files which they aren't used in.  */

static __inline__ vec_float4 spu_re (vec_float4 ra) __attribute__((__always_inline__));
static __inline__ vec_float4 spu_rsqrte (vec_float4 ra) __attribute__((__always_inline__));

static __inline__ vec_float4
spu_re (vec_float4 ra)
{
  return (vec_float4) si_fi ((qword) (ra), si_frest ((qword) (ra)));
}
static __inline__ vec_float4
spu_rsqrte (vec_float4 ra)
{
  return (vec_float4) si_fi ((qword) (ra), si_frsqest ((qword) (ra)));
}

/* composite intrinsics */
static __inline__ void spu_mfcdma32(volatile void *ls, unsigned int ea, unsigned int size, unsigned int tagid, unsigned int cmd) __attribute__((__always_inline__));
static __inline__ void spu_mfcdma64(volatile void *ls, unsigned int eahi, unsigned int ealow, unsigned int size, unsigned int tagid, unsigned int cmd) __attribute__((__always_inline__));
static __inline__ unsigned int spu_mfcstat(unsigned int type) __attribute__((__always_inline__));

static __inline__ void
spu_mfcdma32(volatile void *ls, unsigned int ea, unsigned int size, unsigned int tagid, unsigned int cmd)
{
      si_wrch(MFC_LSA,si_from_ptr(ls));
      si_wrch(MFC_EAL,si_from_uint(ea));
      si_wrch(MFC_Size,si_from_uint(size));
      si_wrch(MFC_TagID,si_from_uint(tagid));
      si_wrch(MFC_Cmd,si_from_uint(cmd));
}
static __inline__ void
spu_mfcdma64(volatile void *ls, unsigned int eahi, unsigned int ealow, unsigned int size, unsigned int tagid, unsigned int cmd)
{
      si_wrch(MFC_LSA,si_from_ptr(ls));
      si_wrch(MFC_EAH,si_from_uint(eahi));
      si_wrch(MFC_EAL,si_from_uint(ealow));
      si_wrch(MFC_Size,si_from_uint(size));
      si_wrch(MFC_TagID,si_from_uint(tagid));
      si_wrch(MFC_Cmd,si_from_uint(cmd));
}
static __inline__ unsigned int
spu_mfcstat(unsigned int type)
{
      si_wrch(MFC_WrTagUpdate,si_from_uint(type));
      return si_to_uint(si_rdch(MFC_RdTagStat));
}
#ifdef __cplusplus

}
#endif  /* __cplusplus */

#endif /* SPUINTRIN_H */

