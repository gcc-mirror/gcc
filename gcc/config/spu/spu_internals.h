/* Definitions of Synergistic Processing Unit (SPU). */
/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* As a special exception, if you include this header file into source files 
   compiled by GCC, this header file does not by itself cause  the resulting 
   executable to be covered by the GNU General Public License.  This exception 
   does not however invalidate any other reasons why the executable file might be 
   covered by the GNU General Public License.  */ 


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

#ifndef __cplusplus

/* generic spu_* intrinisics */ 

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
#define spu_extract(ra,pos)       __builtin_spu_extract(ra,pos) 
#define spu_insert(scalar,ra,pos) __builtin_spu_insert(scalar,ra,pos) 
#define spu_promote(scalar,pos)   __builtin_spu_promote(scalar,pos) 

#else /* __cplusplus */

/* A bit of a hack...  Float conversion needs an immediate operand.
 * always_inline doesn't help because the compiler generates an error
 * before inlining happens. */
static inline vec_float4 __hack_spu_convtf (vec_int4, vec_float4, vec_float4) __attribute__((__always_inline__));
static inline vec_float4 __hack_spu_convtf (vec_uint4, vec_float4, vec_float4) __attribute__((__always_inline__));
static inline vec_float4
__hack_spu_convtf (vec_int4 ra, vec_float4 from_signed, vec_float4 from_unsigned)
{
  (void)ra;
  (void)from_unsigned;
  return from_signed;
}
static inline vec_float4
__hack_spu_convtf (vec_uint4 ra, vec_float4 from_signed, vec_float4 from_unsigned)
{
  (void)ra;
  (void)from_signed;
  return from_unsigned;
}
#define spu_convtf(ra,imm) \
  __hack_spu_convtf((ra), \
	            __builtin_spu_convtf_1((vec_int4)(ra), (imm)), \
		    __builtin_spu_convtf_0((vec_uint4)(ra), (imm)))

/* The following defines and functions were created automatically from
 * spu_builtins.def. */
#define spu_convts(a, b)	__builtin_spu_convts (a, b)
#define spu_convtu(a, b)	__builtin_spu_convtu (a, b)
#define spu_roundtf(a)		__builtin_spu_roundtf (a)
#define spu_mulh(a, b)		__builtin_spu_mulh (a, b)
#define spu_mulsr(a, b)		__builtin_spu_mulsr (a, b)
#define spu_frest(a)		__builtin_spu_frest (a)
#define spu_frsqest(a)		__builtin_spu_frsqest (a)
#define spu_nmadd(a, b, c)	__builtin_spu_nmadd (a, b, c)
#define spu_absd(a, b)		__builtin_spu_absd (a, b)
#define spu_avg(a, b)		__builtin_spu_avg (a, b)
#define spu_sumb(a, b)		__builtin_spu_sumb (a, b)
#define spu_bisled(a)		__builtin_spu_bisled (a, 0)
#define spu_bisled_d(a)		__builtin_spu_bisled_d (a, 0)
#define spu_bisled_e(a)		__builtin_spu_bisled_e (a, 0)
#define spu_cmpabseq(a, b)	__builtin_spu_cmpabseq (a, b)
#define spu_cmpabsgt(a, b)	__builtin_spu_cmpabsgt (a, b)

static inline vec_short8 spu_extend (vec_char16 a) __attribute__((__always_inline__));
static inline vec_int4 spu_extend (vec_short8 a) __attribute__((__always_inline__));
static inline vec_llong2 spu_extend (vec_int4 a) __attribute__((__always_inline__));
static inline vec_double2 spu_extend (vec_float4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_add (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_add (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_add (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_add (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_float4 spu_add (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_add (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_add (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_short8 spu_add (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_add (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_add (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_addx (vec_int4 a, vec_int4 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_addx (vec_uint4 a, vec_uint4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_int4 spu_genc (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_genc (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_gencx (vec_int4 a, vec_int4 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_gencx (vec_uint4 a, vec_uint4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_int4 spu_madd (vec_short8 a, vec_short8 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_float4 spu_madd (vec_float4 a, vec_float4 b, vec_float4 c) __attribute__((__always_inline__));
static inline vec_double2 spu_madd (vec_double2 a, vec_double2 b, vec_double2 c) __attribute__((__always_inline__));
static inline vec_float4 spu_msub (vec_float4 a, vec_float4 b, vec_float4 c) __attribute__((__always_inline__));
static inline vec_double2 spu_msub (vec_double2 a, vec_double2 b, vec_double2 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_mhhadd (vec_ushort8 a, vec_ushort8 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_int4 spu_mhhadd (vec_short8 a, vec_short8 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_mule (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_int4 spu_mule (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_float4 spu_mul (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_mul (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_mulo (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_mulo (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_int4 spu_mulo (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_mulo (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_float4 spu_nmsub (vec_float4 a, vec_float4 b, vec_float4 c) __attribute__((__always_inline__));
static inline vec_double2 spu_nmsub (vec_double2 a, vec_double2 b, vec_double2 c) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sub (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_sub (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_sub (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_sub (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_float4 spu_sub (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_sub (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sub (unsigned short a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_sub (short a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_sub (unsigned int a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_sub (int a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_subx (vec_uint4 a, vec_uint4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_int4 spu_subx (vec_int4 a, vec_int4 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_genb (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_genb (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_genbx (vec_uint4 a, vec_uint4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_int4 spu_genbx (vec_int4 a, vec_int4 b, vec_int4 c) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpeq (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpeq (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpeq (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpeq (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpeq (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpeq (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpeq (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpeq (vec_uchar16 a, unsigned char b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpeq (vec_char16 a, signed char b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpeq (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpeq (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpeq (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpeq (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpgt (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpgt (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpgt (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpgt (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpgt (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpgt (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpgt (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpgt (vec_uchar16 a, unsigned char b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cmpgt (vec_char16 a, signed char b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpgt (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_cmpgt (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpgt (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_cmpgt (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline void spu_hcmpeq (int a, int b) __attribute__((__always_inline__));
static inline void spu_hcmpeq (unsigned int a, unsigned int b) __attribute__((__always_inline__));
static inline void spu_hcmpgt (int a, int b) __attribute__((__always_inline__));
static inline void spu_hcmpgt (unsigned int a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cntb (vec_char16 a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_cntb (vec_uchar16 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_cntlz (vec_int4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_cntlz (vec_uint4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_cntlz (vec_float4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_int4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_uint4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_short8 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_ushort8 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_char16 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_uchar16 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_gather (vec_float4 a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_maskb (unsigned short a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_maskb (short a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_maskb (unsigned int a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_maskb (int a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (unsigned char a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (signed char a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (char a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (unsigned short a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (short a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (unsigned int a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_maskh (int a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (unsigned char a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (signed char a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (char a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (unsigned short a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (short a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (unsigned int a) __attribute__((__always_inline__));
static inline vec_uint4 spu_maskw (int a) __attribute__((__always_inline__));
static inline vec_llong2 spu_sel (vec_llong2 a, vec_llong2 b, vec_ullong2 c) __attribute__((__always_inline__));
static inline vec_ullong2 spu_sel (vec_ullong2 a, vec_ullong2 b, vec_ullong2 c) __attribute__((__always_inline__));
static inline vec_int4 spu_sel (vec_int4 a, vec_int4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_sel (vec_uint4 a, vec_uint4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_short8 spu_sel (vec_short8 a, vec_short8 b, vec_ushort8 c) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sel (vec_ushort8 a, vec_ushort8 b, vec_ushort8 c) __attribute__((__always_inline__));
static inline vec_char16 spu_sel (vec_char16 a, vec_char16 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_uchar16 spu_sel (vec_uchar16 a, vec_uchar16 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_float4 spu_sel (vec_float4 a, vec_float4 b, vec_uint4 c) __attribute__((__always_inline__));
static inline vec_double2 spu_sel (vec_double2 a, vec_double2 b, vec_ullong2 c) __attribute__((__always_inline__));
static inline vec_llong2 spu_sel (vec_llong2 a, vec_llong2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_ullong2 spu_sel (vec_ullong2 a, vec_ullong2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_int4 spu_sel (vec_int4 a, vec_int4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_sel (vec_uint4 a, vec_uint4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_short8 spu_sel (vec_short8 a, vec_short8 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sel (vec_ushort8 a, vec_ushort8 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_float4 spu_sel (vec_float4 a, vec_float4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_double2 spu_sel (vec_double2 a, vec_double2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_uchar16 spu_shuffle (vec_uchar16 a, vec_uchar16 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_char16 spu_shuffle (vec_char16 a, vec_char16 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_ushort8 spu_shuffle (vec_ushort8 a, vec_ushort8 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_short8 spu_shuffle (vec_short8 a, vec_short8 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_uint4 spu_shuffle (vec_uint4 a, vec_uint4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_int4 spu_shuffle (vec_int4 a, vec_int4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_ullong2 spu_shuffle (vec_ullong2 a, vec_ullong2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_llong2 spu_shuffle (vec_llong2 a, vec_llong2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_float4 spu_shuffle (vec_float4 a, vec_float4 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_double2 spu_shuffle (vec_double2 a, vec_double2 b, vec_uchar16 c) __attribute__((__always_inline__));
static inline vec_uchar16 spu_and (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_char16 spu_and (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_and (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_and (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_and (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_and (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_and (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_and (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_float4 spu_and (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_and (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_and (vec_uchar16 a, unsigned char b) __attribute__((__always_inline__));
static inline vec_char16 spu_and (vec_char16 a, signed char b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_and (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_short8 spu_and (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_and (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_and (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_andc (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_andc (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_andc (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_andc (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_short8 spu_andc (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_andc (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_char16 spu_andc (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_andc (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_float4 spu_andc (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_andc (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_eqv (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_eqv (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_eqv (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_eqv (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_short8 spu_eqv (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_eqv (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_char16 spu_eqv (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_eqv (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_float4 spu_eqv (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_eqv (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_nand (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_nand (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_nand (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_nand (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_short8 spu_nand (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_nand (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_char16 spu_nand (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_nand (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_float4 spu_nand (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_nand (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_nor (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_nor (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_nor (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_nor (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_short8 spu_nor (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_nor (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_char16 spu_nor (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_nor (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_float4 spu_nor (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_nor (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_or (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_char16 spu_or (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_or (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_or (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_or (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_or (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_or (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_or (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_float4 spu_or (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_or (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_or (vec_uchar16 a, unsigned char b) __attribute__((__always_inline__));
static inline vec_char16 spu_or (vec_char16 a, signed char b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_or (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_short8 spu_or (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_or (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_or (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_orc (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_orc (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_orc (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_orc (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_short8 spu_orc (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_orc (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_char16 spu_orc (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_orc (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_float4 spu_orc (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_orc (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_int4 spu_orx (vec_int4 a) __attribute__((__always_inline__));
static inline vec_uint4 spu_orx (vec_uint4 a) __attribute__((__always_inline__));
static inline vec_uchar16 spu_xor (vec_uchar16 a, vec_uchar16 b) __attribute__((__always_inline__));
static inline vec_char16 spu_xor (vec_char16 a, vec_char16 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_xor (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_xor (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_xor (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_xor (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_xor (vec_ullong2 a, vec_ullong2 b) __attribute__((__always_inline__));
static inline vec_llong2 spu_xor (vec_llong2 a, vec_llong2 b) __attribute__((__always_inline__));
static inline vec_float4 spu_xor (vec_float4 a, vec_float4 b) __attribute__((__always_inline__));
static inline vec_double2 spu_xor (vec_double2 a, vec_double2 b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_xor (vec_uchar16 a, unsigned char b) __attribute__((__always_inline__));
static inline vec_char16 spu_xor (vec_char16 a, signed char b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_xor (vec_ushort8 a, unsigned short b) __attribute__((__always_inline__));
static inline vec_short8 spu_xor (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_xor (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_xor (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rl (vec_ushort8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_rl (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rl (vec_uint4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_rl (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rl (vec_ushort8 a, short b) __attribute__((__always_inline__));
static inline vec_short8 spu_rl (vec_short8 a, short b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rl (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rl (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlqw (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlqw (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlqw (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlqw (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlqw (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlqw (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlqw (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlqw (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlqw (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlqw (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlqwbyte (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlqwbyte (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlqwbyte (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlqwbyte (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlqwbyte (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlqwbyte (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlqwbyte (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlqwbyte (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlqwbyte (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlqwbyte (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlqwbytebc (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlqwbytebc (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlqwbytebc (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlqwbytebc (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlqwbytebc (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlqwbytebc (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlqwbytebc (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlqwbytebc (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlqwbytebc (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlqwbytebc (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmask (vec_ushort8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmask (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmask (vec_uint4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmask (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmask (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmask (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmask (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmask (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmaska (vec_ushort8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmaska (vec_short8 a, vec_short8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmaska (vec_uint4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmaska (vec_int4 a, vec_int4 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmaska (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmaska (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmaska (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmaska (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlmaskqw (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlmaskqw (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmaskqw (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmaskqw (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmaskqw (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmaskqw (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlmaskqw (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlmaskqw (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlmaskqw (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlmaskqw (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlmaskqwbyte (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlmaskqwbyte (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmaskqwbyte (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmaskqwbyte (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmaskqwbyte (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmaskqwbyte (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlmaskqwbyte (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlmaskqwbyte (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlmaskqwbyte (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlmaskqwbyte (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_rlmaskqwbytebc (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_rlmaskqwbytebc (vec_char16 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_rlmaskqwbytebc (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_rlmaskqwbytebc (vec_short8 a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_rlmaskqwbytebc (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_rlmaskqwbytebc (vec_int4 a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_rlmaskqwbytebc (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_rlmaskqwbytebc (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_rlmaskqwbytebc (vec_float4 a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_rlmaskqwbytebc (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sl (vec_ushort8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_short8 spu_sl (vec_short8 a, vec_ushort8 b) __attribute__((__always_inline__));
static inline vec_uint4 spu_sl (vec_uint4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_int4 spu_sl (vec_int4 a, vec_uint4 b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_sl (vec_ushort8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_short8 spu_sl (vec_short8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_sl (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_sl (vec_int4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_slqw (vec_llong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_slqw (vec_ullong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_slqw (vec_int4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_slqw (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_short8 spu_slqw (vec_short8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_slqw (vec_ushort8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_char16 spu_slqw (vec_char16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_slqw (vec_uchar16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_float4 spu_slqw (vec_float4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_double2 spu_slqw (vec_double2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_slqwbyte (vec_llong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_slqwbyte (vec_ullong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_slqwbyte (vec_int4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_slqwbyte (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_short8 spu_slqwbyte (vec_short8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_slqwbyte (vec_ushort8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_char16 spu_slqwbyte (vec_char16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_slqwbyte (vec_uchar16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_float4 spu_slqwbyte (vec_float4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_double2 spu_slqwbyte (vec_double2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_slqwbytebc (vec_llong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_slqwbytebc (vec_ullong2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_int4 spu_slqwbytebc (vec_int4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_slqwbytebc (vec_uint4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_short8 spu_slqwbytebc (vec_short8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_slqwbytebc (vec_ushort8 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_char16 spu_slqwbytebc (vec_char16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_slqwbytebc (vec_uchar16 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_float4 spu_slqwbytebc (vec_float4 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_double2 spu_slqwbytebc (vec_double2 a, unsigned int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_splats (unsigned char a) __attribute__((__always_inline__));
static inline vec_char16 spu_splats (signed char a) __attribute__((__always_inline__));
static inline vec_char16 spu_splats (char a) __attribute__((__always_inline__));
static inline vec_ushort8 spu_splats (unsigned short a) __attribute__((__always_inline__));
static inline vec_short8 spu_splats (short a) __attribute__((__always_inline__));
static inline vec_uint4 spu_splats (unsigned int a) __attribute__((__always_inline__));
static inline vec_int4 spu_splats (int a) __attribute__((__always_inline__));
static inline vec_ullong2 spu_splats (unsigned long long a) __attribute__((__always_inline__));
static inline vec_llong2 spu_splats (long long a) __attribute__((__always_inline__));
static inline vec_float4 spu_splats (float a) __attribute__((__always_inline__));
static inline vec_double2 spu_splats (double a) __attribute__((__always_inline__));
static inline unsigned char spu_extract (vec_uchar16 a, int b) __attribute__((__always_inline__));
static inline signed char spu_extract (vec_char16 a, int b) __attribute__((__always_inline__));
static inline unsigned short spu_extract (vec_ushort8 a, int b) __attribute__((__always_inline__));
static inline short spu_extract (vec_short8 a, int b) __attribute__((__always_inline__));
static inline unsigned int spu_extract (vec_uint4 a, int b) __attribute__((__always_inline__));
static inline int spu_extract (vec_int4 a, int b) __attribute__((__always_inline__));
static inline unsigned long long spu_extract (vec_ullong2 a, int b) __attribute__((__always_inline__));
static inline long long spu_extract (vec_llong2 a, int b) __attribute__((__always_inline__));
static inline float spu_extract (vec_float4 a, int b) __attribute__((__always_inline__));
static inline double spu_extract (vec_double2 a, int b) __attribute__((__always_inline__));
static inline vec_uchar16 spu_insert (unsigned char a, vec_uchar16 b, int c) __attribute__((__always_inline__));
static inline vec_char16 spu_insert (signed char a, vec_char16 b, int c) __attribute__((__always_inline__));
static inline vec_ushort8 spu_insert (unsigned short a, vec_ushort8 b, int c) __attribute__((__always_inline__));
static inline vec_short8 spu_insert (short a, vec_short8 b, int c) __attribute__((__always_inline__));
static inline vec_uint4 spu_insert (unsigned int a, vec_uint4 b, int c) __attribute__((__always_inline__));
static inline vec_int4 spu_insert (int a, vec_int4 b, int c) __attribute__((__always_inline__));
static inline vec_ullong2 spu_insert (unsigned long long a, vec_ullong2 b, int c) __attribute__((__always_inline__));
static inline vec_llong2 spu_insert (long long a, vec_llong2 b, int c) __attribute__((__always_inline__));
static inline vec_float4 spu_insert (float a, vec_float4 b, int c) __attribute__((__always_inline__));
static inline vec_double2 spu_insert (double a, vec_double2 b, int c) __attribute__((__always_inline__));
static inline vec_uchar16 spu_promote (unsigned char a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_promote (signed char a, int b) __attribute__((__always_inline__));
static inline vec_char16 spu_promote (char a, int b) __attribute__((__always_inline__));
static inline vec_ushort8 spu_promote (unsigned short a, int b) __attribute__((__always_inline__));
static inline vec_short8 spu_promote (short a, int b) __attribute__((__always_inline__));
static inline vec_uint4 spu_promote (unsigned int a, int b) __attribute__((__always_inline__));
static inline vec_int4 spu_promote (int a, int b) __attribute__((__always_inline__));
static inline vec_ullong2 spu_promote (unsigned long long a, int b) __attribute__((__always_inline__));
static inline vec_llong2 spu_promote (long long a, int b) __attribute__((__always_inline__));
static inline vec_float4 spu_promote (float a, int b) __attribute__((__always_inline__));
static inline vec_double2 spu_promote (double a, int b) __attribute__((__always_inline__));

static inline vec_short8
spu_extend (vec_char16 a)
{
  return __builtin_spu_extend_0 (a);
}
static inline vec_int4
spu_extend (vec_short8 a)
{
  return __builtin_spu_extend_1 (a);
}
static inline vec_llong2
spu_extend (vec_int4 a)
{
  return __builtin_spu_extend_2 (a);
}
static inline vec_double2
spu_extend (vec_float4 a)
{
  return __builtin_spu_extend_3 (a);
}
static inline vec_uint4
spu_add (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_add_0 (a, b);
}
static inline vec_int4
spu_add (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_add_1 (a, b);
}
static inline vec_ushort8
spu_add (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_add_2 (a, b);
}
static inline vec_short8
spu_add (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_add_3 (a, b);
}
static inline vec_float4
spu_add (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_add_4 (a, b);
}
static inline vec_double2
spu_add (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_add_5 (a, b);
}
static inline vec_ushort8
spu_add (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_add_6 (a, b);
}
static inline vec_short8
spu_add (vec_short8 a, short b)
{
  return __builtin_spu_add_7 (a, b);
}
static inline vec_uint4
spu_add (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_add_8 (a, b);
}
static inline vec_int4
spu_add (vec_int4 a, int b)
{
  return __builtin_spu_add_9 (a, b);
}
static inline vec_int4
spu_addx (vec_int4 a, vec_int4 b, vec_int4 c)
{
  return __builtin_spu_addx_0 (a, b, c);
}
static inline vec_uint4
spu_addx (vec_uint4 a, vec_uint4 b, vec_uint4 c)
{
  return __builtin_spu_addx_1 (a, b, c);
}
static inline vec_int4
spu_genc (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_genc_0 (a, b);
}
static inline vec_uint4
spu_genc (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_genc_1 (a, b);
}
static inline vec_int4
spu_gencx (vec_int4 a, vec_int4 b, vec_int4 c)
{
  return __builtin_spu_gencx_0 (a, b, c);
}
static inline vec_uint4
spu_gencx (vec_uint4 a, vec_uint4 b, vec_uint4 c)
{
  return __builtin_spu_gencx_1 (a, b, c);
}
static inline vec_int4
spu_madd (vec_short8 a, vec_short8 b, vec_int4 c)
{
  return __builtin_spu_madd_0 (a, b, c);
}
static inline vec_float4
spu_madd (vec_float4 a, vec_float4 b, vec_float4 c)
{
  return __builtin_spu_madd_1 (a, b, c);
}
static inline vec_double2
spu_madd (vec_double2 a, vec_double2 b, vec_double2 c)
{
  return __builtin_spu_madd_2 (a, b, c);
}
static inline vec_float4
spu_msub (vec_float4 a, vec_float4 b, vec_float4 c)
{
  return __builtin_spu_msub_0 (a, b, c);
}
static inline vec_double2
spu_msub (vec_double2 a, vec_double2 b, vec_double2 c)
{
  return __builtin_spu_msub_1 (a, b, c);
}
static inline vec_uint4
spu_mhhadd (vec_ushort8 a, vec_ushort8 b, vec_uint4 c)
{
  return __builtin_spu_mhhadd_0 (a, b, c);
}
static inline vec_int4
spu_mhhadd (vec_short8 a, vec_short8 b, vec_int4 c)
{
  return __builtin_spu_mhhadd_1 (a, b, c);
}
static inline vec_uint4
spu_mule (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_mule_0 (a, b);
}
static inline vec_int4
spu_mule (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_mule_1 (a, b);
}
static inline vec_float4
spu_mul (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_mul_0 (a, b);
}
static inline vec_double2
spu_mul (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_mul_1 (a, b);
}
static inline vec_int4
spu_mulo (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_mulo_0 (a, b);
}
static inline vec_uint4
spu_mulo (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_mulo_1 (a, b);
}
static inline vec_int4
spu_mulo (vec_short8 a, short b)
{
  return __builtin_spu_mulo_2 (a, b);
}
static inline vec_uint4
spu_mulo (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_mulo_3 (a, b);
}
static inline vec_float4
spu_nmsub (vec_float4 a, vec_float4 b, vec_float4 c)
{
  return __builtin_spu_nmsub_0 (a, b, c);
}
static inline vec_double2
spu_nmsub (vec_double2 a, vec_double2 b, vec_double2 c)
{
  return __builtin_spu_nmsub_1 (a, b, c);
}
static inline vec_ushort8
spu_sub (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_sub_0 (a, b);
}
static inline vec_short8
spu_sub (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_sub_1 (a, b);
}
static inline vec_uint4
spu_sub (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_sub_2 (a, b);
}
static inline vec_int4
spu_sub (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_sub_3 (a, b);
}
static inline vec_float4
spu_sub (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_sub_4 (a, b);
}
static inline vec_double2
spu_sub (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_sub_5 (a, b);
}
static inline vec_ushort8
spu_sub (unsigned short a, vec_ushort8 b)
{
  return __builtin_spu_sub_6 (a, b);
}
static inline vec_short8
spu_sub (short a, vec_short8 b)
{
  return __builtin_spu_sub_7 (a, b);
}
static inline vec_uint4
spu_sub (unsigned int a, vec_uint4 b)
{
  return __builtin_spu_sub_8 (a, b);
}
static inline vec_int4
spu_sub (int a, vec_int4 b)
{
  return __builtin_spu_sub_9 (a, b);
}
static inline vec_uint4
spu_subx (vec_uint4 a, vec_uint4 b, vec_uint4 c)
{
  return __builtin_spu_subx_0 (a, b, c);
}
static inline vec_int4
spu_subx (vec_int4 a, vec_int4 b, vec_int4 c)
{
  return __builtin_spu_subx_1 (a, b, c);
}
static inline vec_uint4
spu_genb (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_genb_0 (a, b);
}
static inline vec_int4
spu_genb (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_genb_1 (a, b);
}
static inline vec_uint4
spu_genbx (vec_uint4 a, vec_uint4 b, vec_uint4 c)
{
  return __builtin_spu_genbx_0 (a, b, c);
}
static inline vec_int4
spu_genbx (vec_int4 a, vec_int4 b, vec_int4 c)
{
  return __builtin_spu_genbx_1 (a, b, c);
}
static inline vec_uchar16
spu_cmpeq (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_cmpeq_0 (a, b);
}
static inline vec_uchar16
spu_cmpeq (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_cmpeq_1 (a, b);
}
static inline vec_ushort8
spu_cmpeq (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_cmpeq_2 (a, b);
}
static inline vec_ushort8
spu_cmpeq (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_cmpeq_3 (a, b);
}
static inline vec_uint4
spu_cmpeq (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_cmpeq_4 (a, b);
}
static inline vec_uint4
spu_cmpeq (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_cmpeq_5 (a, b);
}
static inline vec_uint4
spu_cmpeq (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_cmpeq_6 (a, b);
}
static inline vec_uchar16
spu_cmpeq (vec_uchar16 a, unsigned char b)
{
  return __builtin_spu_cmpeq_7 (a, b);
}
static inline vec_uchar16
spu_cmpeq (vec_char16 a, signed char b)
{
  return __builtin_spu_cmpeq_8 (a, b);
}
static inline vec_ushort8
spu_cmpeq (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_cmpeq_9 (a, b);
}
static inline vec_ushort8
spu_cmpeq (vec_short8 a, short b)
{
  return __builtin_spu_cmpeq_10 (a, b);
}
static inline vec_uint4
spu_cmpeq (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_cmpeq_11 (a, b);
}
static inline vec_uint4
spu_cmpeq (vec_int4 a, int b)
{
  return __builtin_spu_cmpeq_12 (a, b);
}
static inline vec_uchar16
spu_cmpgt (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_cmpgt_0 (a, b);
}
static inline vec_uchar16
spu_cmpgt (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_cmpgt_1 (a, b);
}
static inline vec_ushort8
spu_cmpgt (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_cmpgt_2 (a, b);
}
static inline vec_ushort8
spu_cmpgt (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_cmpgt_3 (a, b);
}
static inline vec_uint4
spu_cmpgt (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_cmpgt_4 (a, b);
}
static inline vec_uint4
spu_cmpgt (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_cmpgt_5 (a, b);
}
static inline vec_uint4
spu_cmpgt (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_cmpgt_6 (a, b);
}
static inline vec_uchar16
spu_cmpgt (vec_uchar16 a, unsigned char b)
{
  return __builtin_spu_cmpgt_7 (a, b);
}
static inline vec_uchar16
spu_cmpgt (vec_char16 a, signed char b)
{
  return __builtin_spu_cmpgt_8 (a, b);
}
static inline vec_ushort8
spu_cmpgt (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_cmpgt_9 (a, b);
}
static inline vec_ushort8
spu_cmpgt (vec_short8 a, short b)
{
  return __builtin_spu_cmpgt_10 (a, b);
}
static inline vec_uint4
spu_cmpgt (vec_int4 a, int b)
{
  return __builtin_spu_cmpgt_11 (a, b);
}
static inline vec_uint4
spu_cmpgt (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_cmpgt_12 (a, b);
}
static inline void
spu_hcmpeq (int a, int b)
{
  return __builtin_spu_hcmpeq_0 (a, b);
}
static inline void
spu_hcmpeq (unsigned int a, unsigned int b)
{
  return __builtin_spu_hcmpeq_1 (a, b);
}
static inline void
spu_hcmpgt (int a, int b)
{
  return __builtin_spu_hcmpgt_0 (a, b);
}
static inline void
spu_hcmpgt (unsigned int a, unsigned int b)
{
  return __builtin_spu_hcmpgt_1 (a, b);
}
static inline vec_uchar16
spu_cntb (vec_char16 a)
{
  return __builtin_spu_cntb_0 (a);
}
static inline vec_uchar16
spu_cntb (vec_uchar16 a)
{
  return __builtin_spu_cntb_1 (a);
}
static inline vec_uint4
spu_cntlz (vec_int4 a)
{
  return __builtin_spu_cntlz_0 (a);
}
static inline vec_uint4
spu_cntlz (vec_uint4 a)
{
  return __builtin_spu_cntlz_1 (a);
}
static inline vec_uint4
spu_cntlz (vec_float4 a)
{
  return __builtin_spu_cntlz_2 (a);
}
static inline vec_uint4
spu_gather (vec_int4 a)
{
  return __builtin_spu_gather_0 (a);
}
static inline vec_uint4
spu_gather (vec_uint4 a)
{
  return __builtin_spu_gather_1 (a);
}
static inline vec_uint4
spu_gather (vec_short8 a)
{
  return __builtin_spu_gather_2 (a);
}
static inline vec_uint4
spu_gather (vec_ushort8 a)
{
  return __builtin_spu_gather_3 (a);
}
static inline vec_uint4
spu_gather (vec_char16 a)
{
  return __builtin_spu_gather_4 (a);
}
static inline vec_uint4
spu_gather (vec_uchar16 a)
{
  return __builtin_spu_gather_5 (a);
}
static inline vec_uint4
spu_gather (vec_float4 a)
{
  return __builtin_spu_gather_6 (a);
}
static inline vec_uchar16
spu_maskb (unsigned short a)
{
  return __builtin_spu_maskb_0 (a);
}
static inline vec_uchar16
spu_maskb (short a)
{
  return __builtin_spu_maskb_1 (a);
}
static inline vec_uchar16
spu_maskb (unsigned int a)
{
  return __builtin_spu_maskb_2 (a);
}
static inline vec_uchar16
spu_maskb (int a)
{
  return __builtin_spu_maskb_3 (a);
}
static inline vec_ushort8
spu_maskh (unsigned char a)
{
  return __builtin_spu_maskh_0 (a);
}
static inline vec_ushort8
spu_maskh (signed char a)
{
  return __builtin_spu_maskh_1 (a);
}
static inline vec_ushort8
spu_maskh (char a)
{
  return __builtin_spu_maskh_1 (a);
}
static inline vec_ushort8
spu_maskh (unsigned short a)
{
  return __builtin_spu_maskh_2 (a);
}
static inline vec_ushort8
spu_maskh (short a)
{
  return __builtin_spu_maskh_3 (a);
}
static inline vec_ushort8
spu_maskh (unsigned int a)
{
  return __builtin_spu_maskh_4 (a);
}
static inline vec_ushort8
spu_maskh (int a)
{
  return __builtin_spu_maskh_5 (a);
}
static inline vec_uint4
spu_maskw (unsigned char a)
{
  return __builtin_spu_maskw_0 (a);
}
static inline vec_uint4
spu_maskw (signed char a)
{
  return __builtin_spu_maskw_1 (a);
}
static inline vec_uint4
spu_maskw (char a)
{
  return __builtin_spu_maskw_1 (a);
}
static inline vec_uint4
spu_maskw (unsigned short a)
{
  return __builtin_spu_maskw_2 (a);
}
static inline vec_uint4
spu_maskw (short a)
{
  return __builtin_spu_maskw_3 (a);
}
static inline vec_uint4
spu_maskw (unsigned int a)
{
  return __builtin_spu_maskw_4 (a);
}
static inline vec_uint4
spu_maskw (int a)
{
  return __builtin_spu_maskw_5 (a);
}
static inline vec_llong2
spu_sel (vec_llong2 a, vec_llong2 b, vec_ullong2 c)
{
  return __builtin_spu_sel_0 (a, b, c);
}
static inline vec_ullong2
spu_sel (vec_ullong2 a, vec_ullong2 b, vec_ullong2 c)
{
  return __builtin_spu_sel_1 (a, b, c);
}
static inline vec_int4
spu_sel (vec_int4 a, vec_int4 b, vec_uint4 c)
{
  return __builtin_spu_sel_2 (a, b, c);
}
static inline vec_uint4
spu_sel (vec_uint4 a, vec_uint4 b, vec_uint4 c)
{
  return __builtin_spu_sel_3 (a, b, c);
}
static inline vec_short8
spu_sel (vec_short8 a, vec_short8 b, vec_ushort8 c)
{
  return __builtin_spu_sel_4 (a, b, c);
}
static inline vec_ushort8
spu_sel (vec_ushort8 a, vec_ushort8 b, vec_ushort8 c)
{
  return __builtin_spu_sel_5 (a, b, c);
}
static inline vec_char16
spu_sel (vec_char16 a, vec_char16 b, vec_uchar16 c)
{
  return __builtin_spu_sel_6 (a, b, c);
}
static inline vec_uchar16
spu_sel (vec_uchar16 a, vec_uchar16 b, vec_uchar16 c)
{
  return __builtin_spu_sel_7 (a, b, c);
}
static inline vec_float4
spu_sel (vec_float4 a, vec_float4 b, vec_uint4 c)
{
  return __builtin_spu_sel_8 (a, b, c);
}
static inline vec_double2
spu_sel (vec_double2 a, vec_double2 b, vec_ullong2 c)
{
  return __builtin_spu_sel_9 (a, b, c);
}
static inline vec_uchar16
spu_shuffle (vec_uchar16 a, vec_uchar16 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_0 (a, b, c);
}
static inline vec_char16
spu_shuffle (vec_char16 a, vec_char16 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_1 (a, b, c);
}
static inline vec_ushort8
spu_shuffle (vec_ushort8 a, vec_ushort8 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_2 (a, b, c);
}
static inline vec_short8
spu_shuffle (vec_short8 a, vec_short8 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_3 (a, b, c);
}
static inline vec_uint4
spu_shuffle (vec_uint4 a, vec_uint4 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_4 (a, b, c);
}
static inline vec_int4
spu_shuffle (vec_int4 a, vec_int4 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_5 (a, b, c);
}
static inline vec_ullong2
spu_shuffle (vec_ullong2 a, vec_ullong2 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_6 (a, b, c);
}
static inline vec_llong2
spu_shuffle (vec_llong2 a, vec_llong2 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_7 (a, b, c);
}
static inline vec_float4
spu_shuffle (vec_float4 a, vec_float4 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_8 (a, b, c);
}
static inline vec_double2
spu_shuffle (vec_double2 a, vec_double2 b, vec_uchar16 c)
{
  return __builtin_spu_shuffle_9 (a, b, c);
}
static inline vec_uchar16
spu_and (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_and_0 (a, b);
}
static inline vec_char16
spu_and (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_and_1 (a, b);
}
static inline vec_ushort8
spu_and (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_and_2 (a, b);
}
static inline vec_short8
spu_and (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_and_3 (a, b);
}
static inline vec_uint4
spu_and (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_and_4 (a, b);
}
static inline vec_int4
spu_and (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_and_5 (a, b);
}
static inline vec_ullong2
spu_and (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_and_6 (a, b);
}
static inline vec_llong2
spu_and (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_and_7 (a, b);
}
static inline vec_float4
spu_and (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_and_8 (a, b);
}
static inline vec_double2
spu_and (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_and_9 (a, b);
}
static inline vec_uchar16
spu_and (vec_uchar16 a, unsigned char b)
{
  return __builtin_spu_and_10 (a, b);
}
static inline vec_char16
spu_and (vec_char16 a, signed char b)
{
  return __builtin_spu_and_11 (a, b);
}
static inline vec_ushort8
spu_and (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_and_12 (a, b);
}
static inline vec_short8
spu_and (vec_short8 a, short b)
{
  return __builtin_spu_and_13 (a, b);
}
static inline vec_uint4
spu_and (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_and_14 (a, b);
}
static inline vec_int4
spu_and (vec_int4 a, int b)
{
  return __builtin_spu_and_15 (a, b);
}
static inline vec_llong2
spu_andc (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_andc_0 (a, b);
}
static inline vec_ullong2
spu_andc (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_andc_1 (a, b);
}
static inline vec_int4
spu_andc (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_andc_2 (a, b);
}
static inline vec_uint4
spu_andc (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_andc_3 (a, b);
}
static inline vec_short8
spu_andc (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_andc_4 (a, b);
}
static inline vec_ushort8
spu_andc (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_andc_5 (a, b);
}
static inline vec_char16
spu_andc (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_andc_6 (a, b);
}
static inline vec_uchar16
spu_andc (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_andc_7 (a, b);
}
static inline vec_float4
spu_andc (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_andc_8 (a, b);
}
static inline vec_double2
spu_andc (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_andc_9 (a, b);
}
static inline vec_llong2
spu_eqv (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_eqv_0 (a, b);
}
static inline vec_ullong2
spu_eqv (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_eqv_1 (a, b);
}
static inline vec_int4
spu_eqv (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_eqv_2 (a, b);
}
static inline vec_uint4
spu_eqv (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_eqv_3 (a, b);
}
static inline vec_short8
spu_eqv (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_eqv_4 (a, b);
}
static inline vec_ushort8
spu_eqv (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_eqv_5 (a, b);
}
static inline vec_char16
spu_eqv (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_eqv_6 (a, b);
}
static inline vec_uchar16
spu_eqv (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_eqv_7 (a, b);
}
static inline vec_float4
spu_eqv (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_eqv_8 (a, b);
}
static inline vec_double2
spu_eqv (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_eqv_9 (a, b);
}
static inline vec_llong2
spu_nand (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_nand_0 (a, b);
}
static inline vec_ullong2
spu_nand (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_nand_1 (a, b);
}
static inline vec_int4
spu_nand (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_nand_2 (a, b);
}
static inline vec_uint4
spu_nand (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_nand_3 (a, b);
}
static inline vec_short8
spu_nand (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_nand_4 (a, b);
}
static inline vec_ushort8
spu_nand (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_nand_5 (a, b);
}
static inline vec_char16
spu_nand (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_nand_6 (a, b);
}
static inline vec_uchar16
spu_nand (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_nand_7 (a, b);
}
static inline vec_float4
spu_nand (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_nand_8 (a, b);
}
static inline vec_double2
spu_nand (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_nand_9 (a, b);
}
static inline vec_llong2
spu_nor (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_nor_0 (a, b);
}
static inline vec_ullong2
spu_nor (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_nor_1 (a, b);
}
static inline vec_int4
spu_nor (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_nor_2 (a, b);
}
static inline vec_uint4
spu_nor (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_nor_3 (a, b);
}
static inline vec_short8
spu_nor (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_nor_4 (a, b);
}
static inline vec_ushort8
spu_nor (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_nor_5 (a, b);
}
static inline vec_char16
spu_nor (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_nor_6 (a, b);
}
static inline vec_uchar16
spu_nor (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_nor_7 (a, b);
}
static inline vec_float4
spu_nor (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_nor_8 (a, b);
}
static inline vec_double2
spu_nor (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_nor_9 (a, b);
}
static inline vec_uchar16
spu_or (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_or_0 (a, b);
}
static inline vec_char16
spu_or (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_or_1 (a, b);
}
static inline vec_ushort8
spu_or (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_or_2 (a, b);
}
static inline vec_short8
spu_or (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_or_3 (a, b);
}
static inline vec_uint4
spu_or (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_or_4 (a, b);
}
static inline vec_int4
spu_or (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_or_5 (a, b);
}
static inline vec_ullong2
spu_or (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_or_6 (a, b);
}
static inline vec_llong2
spu_or (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_or_7 (a, b);
}
static inline vec_float4
spu_or (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_or_8 (a, b);
}
static inline vec_double2
spu_or (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_or_9 (a, b);
}
static inline vec_uchar16
spu_or (vec_uchar16 a, unsigned char b)
{
  return __builtin_spu_or_10 (a, b);
}
static inline vec_char16
spu_or (vec_char16 a, signed char b)
{
  return __builtin_spu_or_11 (a, b);
}
static inline vec_ushort8
spu_or (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_or_12 (a, b);
}
static inline vec_short8
spu_or (vec_short8 a, short b)
{
  return __builtin_spu_or_13 (a, b);
}
static inline vec_uint4
spu_or (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_or_14 (a, b);
}
static inline vec_int4
spu_or (vec_int4 a, int b)
{
  return __builtin_spu_or_15 (a, b);
}
static inline vec_llong2
spu_orc (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_orc_0 (a, b);
}
static inline vec_ullong2
spu_orc (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_orc_1 (a, b);
}
static inline vec_int4
spu_orc (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_orc_2 (a, b);
}
static inline vec_uint4
spu_orc (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_orc_3 (a, b);
}
static inline vec_short8
spu_orc (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_orc_4 (a, b);
}
static inline vec_ushort8
spu_orc (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_orc_5 (a, b);
}
static inline vec_char16
spu_orc (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_orc_6 (a, b);
}
static inline vec_uchar16
spu_orc (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_orc_7 (a, b);
}
static inline vec_float4
spu_orc (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_orc_8 (a, b);
}
static inline vec_double2
spu_orc (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_orc_9 (a, b);
}
static inline vec_int4
spu_orx (vec_int4 a)
{
  return __builtin_spu_orx_0 (a);
}
static inline vec_uint4
spu_orx (vec_uint4 a)
{
  return __builtin_spu_orx_1 (a);
}
static inline vec_uchar16
spu_xor (vec_uchar16 a, vec_uchar16 b)
{
  return __builtin_spu_xor_0 (a, b);
}
static inline vec_char16
spu_xor (vec_char16 a, vec_char16 b)
{
  return __builtin_spu_xor_1 (a, b);
}
static inline vec_ushort8
spu_xor (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_xor_2 (a, b);
}
static inline vec_short8
spu_xor (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_xor_3 (a, b);
}
static inline vec_uint4
spu_xor (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_xor_4 (a, b);
}
static inline vec_int4
spu_xor (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_xor_5 (a, b);
}
static inline vec_ullong2
spu_xor (vec_ullong2 a, vec_ullong2 b)
{
  return __builtin_spu_xor_6 (a, b);
}
static inline vec_llong2
spu_xor (vec_llong2 a, vec_llong2 b)
{
  return __builtin_spu_xor_7 (a, b);
}
static inline vec_float4
spu_xor (vec_float4 a, vec_float4 b)
{
  return __builtin_spu_xor_8 (a, b);
}
static inline vec_double2
spu_xor (vec_double2 a, vec_double2 b)
{
  return __builtin_spu_xor_9 (a, b);
}
static inline vec_uchar16
spu_xor (vec_uchar16 a, unsigned char b)
{
  return __builtin_spu_xor_10 (a, b);
}
static inline vec_char16
spu_xor (vec_char16 a, signed char b)
{
  return __builtin_spu_xor_11 (a, b);
}
static inline vec_ushort8
spu_xor (vec_ushort8 a, unsigned short b)
{
  return __builtin_spu_xor_12 (a, b);
}
static inline vec_short8
spu_xor (vec_short8 a, short b)
{
  return __builtin_spu_xor_13 (a, b);
}
static inline vec_uint4
spu_xor (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_xor_14 (a, b);
}
static inline vec_int4
spu_xor (vec_int4 a, int b)
{
  return __builtin_spu_xor_15 (a, b);
}
static inline vec_ushort8
spu_rl (vec_ushort8 a, vec_short8 b)
{
  return __builtin_spu_rl_0 (a, b);
}
static inline vec_short8
spu_rl (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_rl_1 (a, b);
}
static inline vec_uint4
spu_rl (vec_uint4 a, vec_int4 b)
{
  return __builtin_spu_rl_2 (a, b);
}
static inline vec_int4
spu_rl (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_rl_3 (a, b);
}
static inline vec_ushort8
spu_rl (vec_ushort8 a, short b)
{
  return __builtin_spu_rl_4 (a, b);
}
static inline vec_short8
spu_rl (vec_short8 a, short b)
{
  return __builtin_spu_rl_5 (a, b);
}
static inline vec_uint4
spu_rl (vec_uint4 a, int b)
{
  return __builtin_spu_rl_6 (a, b);
}
static inline vec_int4
spu_rl (vec_int4 a, int b)
{
  return __builtin_spu_rl_7 (a, b);
}
static inline vec_uchar16
spu_rlqw (vec_uchar16 a, int b)
{
  return __builtin_spu_rlqw_0 (a, b);
}
static inline vec_char16
spu_rlqw (vec_char16 a, int b)
{
  return __builtin_spu_rlqw_1 (a, b);
}
static inline vec_ushort8
spu_rlqw (vec_ushort8 a, int b)
{
  return __builtin_spu_rlqw_2 (a, b);
}
static inline vec_short8
spu_rlqw (vec_short8 a, int b)
{
  return __builtin_spu_rlqw_3 (a, b);
}
static inline vec_uint4
spu_rlqw (vec_uint4 a, int b)
{
  return __builtin_spu_rlqw_4 (a, b);
}
static inline vec_int4
spu_rlqw (vec_int4 a, int b)
{
  return __builtin_spu_rlqw_5 (a, b);
}
static inline vec_ullong2
spu_rlqw (vec_ullong2 a, int b)
{
  return __builtin_spu_rlqw_6 (a, b);
}
static inline vec_llong2
spu_rlqw (vec_llong2 a, int b)
{
  return __builtin_spu_rlqw_7 (a, b);
}
static inline vec_float4
spu_rlqw (vec_float4 a, int b)
{
  return __builtin_spu_rlqw_8 (a, b);
}
static inline vec_double2
spu_rlqw (vec_double2 a, int b)
{
  return __builtin_spu_rlqw_9 (a, b);
}
static inline vec_uchar16
spu_rlqwbyte (vec_uchar16 a, int b)
{
  return __builtin_spu_rlqwbyte_0 (a, b);
}
static inline vec_char16
spu_rlqwbyte (vec_char16 a, int b)
{
  return __builtin_spu_rlqwbyte_1 (a, b);
}
static inline vec_ushort8
spu_rlqwbyte (vec_ushort8 a, int b)
{
  return __builtin_spu_rlqwbyte_2 (a, b);
}
static inline vec_short8
spu_rlqwbyte (vec_short8 a, int b)
{
  return __builtin_spu_rlqwbyte_3 (a, b);
}
static inline vec_uint4
spu_rlqwbyte (vec_uint4 a, int b)
{
  return __builtin_spu_rlqwbyte_4 (a, b);
}
static inline vec_int4
spu_rlqwbyte (vec_int4 a, int b)
{
  return __builtin_spu_rlqwbyte_5 (a, b);
}
static inline vec_ullong2
spu_rlqwbyte (vec_ullong2 a, int b)
{
  return __builtin_spu_rlqwbyte_6 (a, b);
}
static inline vec_llong2
spu_rlqwbyte (vec_llong2 a, int b)
{
  return __builtin_spu_rlqwbyte_7 (a, b);
}
static inline vec_float4
spu_rlqwbyte (vec_float4 a, int b)
{
  return __builtin_spu_rlqwbyte_8 (a, b);
}
static inline vec_double2
spu_rlqwbyte (vec_double2 a, int b)
{
  return __builtin_spu_rlqwbyte_9 (a, b);
}
static inline vec_uchar16
spu_rlqwbytebc (vec_uchar16 a, int b)
{
  return __builtin_spu_rlqwbytebc_0 (a, b);
}
static inline vec_char16
spu_rlqwbytebc (vec_char16 a, int b)
{
  return __builtin_spu_rlqwbytebc_1 (a, b);
}
static inline vec_ushort8
spu_rlqwbytebc (vec_ushort8 a, int b)
{
  return __builtin_spu_rlqwbytebc_2 (a, b);
}
static inline vec_short8
spu_rlqwbytebc (vec_short8 a, int b)
{
  return __builtin_spu_rlqwbytebc_3 (a, b);
}
static inline vec_uint4
spu_rlqwbytebc (vec_uint4 a, int b)
{
  return __builtin_spu_rlqwbytebc_4 (a, b);
}
static inline vec_int4
spu_rlqwbytebc (vec_int4 a, int b)
{
  return __builtin_spu_rlqwbytebc_5 (a, b);
}
static inline vec_ullong2
spu_rlqwbytebc (vec_ullong2 a, int b)
{
  return __builtin_spu_rlqwbytebc_6 (a, b);
}
static inline vec_llong2
spu_rlqwbytebc (vec_llong2 a, int b)
{
  return __builtin_spu_rlqwbytebc_7 (a, b);
}
static inline vec_float4
spu_rlqwbytebc (vec_float4 a, int b)
{
  return __builtin_spu_rlqwbytebc_8 (a, b);
}
static inline vec_double2
spu_rlqwbytebc (vec_double2 a, int b)
{
  return __builtin_spu_rlqwbytebc_9 (a, b);
}
static inline vec_ushort8
spu_rlmask (vec_ushort8 a, vec_short8 b)
{
  return __builtin_spu_rlmask_0 (a, b);
}
static inline vec_short8
spu_rlmask (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_rlmask_1 (a, b);
}
static inline vec_uint4
spu_rlmask (vec_uint4 a, vec_int4 b)
{
  return __builtin_spu_rlmask_2 (a, b);
}
static inline vec_int4
spu_rlmask (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_rlmask_3 (a, b);
}
static inline vec_ushort8
spu_rlmask (vec_ushort8 a, int b)
{
  return __builtin_spu_rlmask_4 (a, b);
}
static inline vec_short8
spu_rlmask (vec_short8 a, int b)
{
  return __builtin_spu_rlmask_5 (a, b);
}
static inline vec_uint4
spu_rlmask (vec_uint4 a, int b)
{
  return __builtin_spu_rlmask_6 (a, b);
}
static inline vec_int4
spu_rlmask (vec_int4 a, int b)
{
  return __builtin_spu_rlmask_7 (a, b);
}
static inline vec_ushort8
spu_rlmaska (vec_ushort8 a, vec_short8 b)
{
  return __builtin_spu_rlmaska_0 (a, b);
}
static inline vec_short8
spu_rlmaska (vec_short8 a, vec_short8 b)
{
  return __builtin_spu_rlmaska_1 (a, b);
}
static inline vec_uint4
spu_rlmaska (vec_uint4 a, vec_int4 b)
{
  return __builtin_spu_rlmaska_2 (a, b);
}
static inline vec_int4
spu_rlmaska (vec_int4 a, vec_int4 b)
{
  return __builtin_spu_rlmaska_3 (a, b);
}
static inline vec_ushort8
spu_rlmaska (vec_ushort8 a, int b)
{
  return __builtin_spu_rlmaska_4 (a, b);
}
static inline vec_short8
spu_rlmaska (vec_short8 a, int b)
{
  return __builtin_spu_rlmaska_5 (a, b);
}
static inline vec_uint4
spu_rlmaska (vec_uint4 a, int b)
{
  return __builtin_spu_rlmaska_6 (a, b);
}
static inline vec_int4
spu_rlmaska (vec_int4 a, int b)
{
  return __builtin_spu_rlmaska_7 (a, b);
}
static inline vec_uchar16
spu_rlmaskqw (vec_uchar16 a, int b)
{
  return __builtin_spu_rlmaskqw_0 (a, b);
}
static inline vec_char16
spu_rlmaskqw (vec_char16 a, int b)
{
  return __builtin_spu_rlmaskqw_1 (a, b);
}
static inline vec_ushort8
spu_rlmaskqw (vec_ushort8 a, int b)
{
  return __builtin_spu_rlmaskqw_2 (a, b);
}
static inline vec_short8
spu_rlmaskqw (vec_short8 a, int b)
{
  return __builtin_spu_rlmaskqw_3 (a, b);
}
static inline vec_uint4
spu_rlmaskqw (vec_uint4 a, int b)
{
  return __builtin_spu_rlmaskqw_4 (a, b);
}
static inline vec_int4
spu_rlmaskqw (vec_int4 a, int b)
{
  return __builtin_spu_rlmaskqw_5 (a, b);
}
static inline vec_ullong2
spu_rlmaskqw (vec_ullong2 a, int b)
{
  return __builtin_spu_rlmaskqw_6 (a, b);
}
static inline vec_llong2
spu_rlmaskqw (vec_llong2 a, int b)
{
  return __builtin_spu_rlmaskqw_7 (a, b);
}
static inline vec_float4
spu_rlmaskqw (vec_float4 a, int b)
{
  return __builtin_spu_rlmaskqw_8 (a, b);
}
static inline vec_double2
spu_rlmaskqw (vec_double2 a, int b)
{
  return __builtin_spu_rlmaskqw_9 (a, b);
}
static inline vec_uchar16
spu_rlmaskqwbyte (vec_uchar16 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_0 (a, b);
}
static inline vec_char16
spu_rlmaskqwbyte (vec_char16 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_1 (a, b);
}
static inline vec_ushort8
spu_rlmaskqwbyte (vec_ushort8 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_2 (a, b);
}
static inline vec_short8
spu_rlmaskqwbyte (vec_short8 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_3 (a, b);
}
static inline vec_uint4
spu_rlmaskqwbyte (vec_uint4 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_4 (a, b);
}
static inline vec_int4
spu_rlmaskqwbyte (vec_int4 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_5 (a, b);
}
static inline vec_ullong2
spu_rlmaskqwbyte (vec_ullong2 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_6 (a, b);
}
static inline vec_llong2
spu_rlmaskqwbyte (vec_llong2 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_7 (a, b);
}
static inline vec_float4
spu_rlmaskqwbyte (vec_float4 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_8 (a, b);
}
static inline vec_double2
spu_rlmaskqwbyte (vec_double2 a, int b)
{
  return __builtin_spu_rlmaskqwbyte_9 (a, b);
}
static inline vec_uchar16
spu_rlmaskqwbytebc (vec_uchar16 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_0 (a, b);
}
static inline vec_char16
spu_rlmaskqwbytebc (vec_char16 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_1 (a, b);
}
static inline vec_ushort8
spu_rlmaskqwbytebc (vec_ushort8 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_2 (a, b);
}
static inline vec_short8
spu_rlmaskqwbytebc (vec_short8 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_3 (a, b);
}
static inline vec_uint4
spu_rlmaskqwbytebc (vec_uint4 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_4 (a, b);
}
static inline vec_int4
spu_rlmaskqwbytebc (vec_int4 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_5 (a, b);
}
static inline vec_ullong2
spu_rlmaskqwbytebc (vec_ullong2 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_6 (a, b);
}
static inline vec_llong2
spu_rlmaskqwbytebc (vec_llong2 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_7 (a, b);
}
static inline vec_float4
spu_rlmaskqwbytebc (vec_float4 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_8 (a, b);
}
static inline vec_double2
spu_rlmaskqwbytebc (vec_double2 a, int b)
{
  return __builtin_spu_rlmaskqwbytebc_9 (a, b);
}
static inline vec_ushort8
spu_sl (vec_ushort8 a, vec_ushort8 b)
{
  return __builtin_spu_sl_0 (a, b);
}
static inline vec_short8
spu_sl (vec_short8 a, vec_ushort8 b)
{
  return __builtin_spu_sl_1 (a, b);
}
static inline vec_uint4
spu_sl (vec_uint4 a, vec_uint4 b)
{
  return __builtin_spu_sl_2 (a, b);
}
static inline vec_int4
spu_sl (vec_int4 a, vec_uint4 b)
{
  return __builtin_spu_sl_3 (a, b);
}
static inline vec_ushort8
spu_sl (vec_ushort8 a, unsigned int b)
{
  return __builtin_spu_sl_4 (a, b);
}
static inline vec_short8
spu_sl (vec_short8 a, unsigned int b)
{
  return __builtin_spu_sl_5 (a, b);
}
static inline vec_uint4
spu_sl (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_sl_6 (a, b);
}
static inline vec_int4
spu_sl (vec_int4 a, unsigned int b)
{
  return __builtin_spu_sl_7 (a, b);
}
static inline vec_llong2
spu_slqw (vec_llong2 a, unsigned int b)
{
  return __builtin_spu_slqw_0 (a, b);
}
static inline vec_ullong2
spu_slqw (vec_ullong2 a, unsigned int b)
{
  return __builtin_spu_slqw_1 (a, b);
}
static inline vec_int4
spu_slqw (vec_int4 a, unsigned int b)
{
  return __builtin_spu_slqw_2 (a, b);
}
static inline vec_uint4
spu_slqw (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_slqw_3 (a, b);
}
static inline vec_short8
spu_slqw (vec_short8 a, unsigned int b)
{
  return __builtin_spu_slqw_4 (a, b);
}
static inline vec_ushort8
spu_slqw (vec_ushort8 a, unsigned int b)
{
  return __builtin_spu_slqw_5 (a, b);
}
static inline vec_char16
spu_slqw (vec_char16 a, unsigned int b)
{
  return __builtin_spu_slqw_6 (a, b);
}
static inline vec_uchar16
spu_slqw (vec_uchar16 a, unsigned int b)
{
  return __builtin_spu_slqw_7 (a, b);
}
static inline vec_float4
spu_slqw (vec_float4 a, unsigned int b)
{
  return __builtin_spu_slqw_8 (a, b);
}
static inline vec_double2
spu_slqw (vec_double2 a, unsigned int b)
{
  return __builtin_spu_slqw_9 (a, b);
}
static inline vec_llong2
spu_slqwbyte (vec_llong2 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_0 (a, b);
}
static inline vec_ullong2
spu_slqwbyte (vec_ullong2 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_1 (a, b);
}
static inline vec_int4
spu_slqwbyte (vec_int4 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_2 (a, b);
}
static inline vec_uint4
spu_slqwbyte (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_3 (a, b);
}
static inline vec_short8
spu_slqwbyte (vec_short8 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_4 (a, b);
}
static inline vec_ushort8
spu_slqwbyte (vec_ushort8 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_5 (a, b);
}
static inline vec_char16
spu_slqwbyte (vec_char16 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_6 (a, b);
}
static inline vec_uchar16
spu_slqwbyte (vec_uchar16 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_7 (a, b);
}
static inline vec_float4
spu_slqwbyte (vec_float4 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_8 (a, b);
}
static inline vec_double2
spu_slqwbyte (vec_double2 a, unsigned int b)
{
  return __builtin_spu_slqwbyte_9 (a, b);
}
static inline vec_llong2
spu_slqwbytebc (vec_llong2 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_0 (a, b);
}
static inline vec_ullong2
spu_slqwbytebc (vec_ullong2 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_1 (a, b);
}
static inline vec_int4
spu_slqwbytebc (vec_int4 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_2 (a, b);
}
static inline vec_uint4
spu_slqwbytebc (vec_uint4 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_3 (a, b);
}
static inline vec_short8
spu_slqwbytebc (vec_short8 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_4 (a, b);
}
static inline vec_ushort8
spu_slqwbytebc (vec_ushort8 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_5 (a, b);
}
static inline vec_char16
spu_slqwbytebc (vec_char16 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_6 (a, b);
}
static inline vec_uchar16
spu_slqwbytebc (vec_uchar16 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_7 (a, b);
}
static inline vec_float4
spu_slqwbytebc (vec_float4 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_8 (a, b);
}
static inline vec_double2
spu_slqwbytebc (vec_double2 a, unsigned int b)
{
  return __builtin_spu_slqwbytebc_9 (a, b);
}
static inline vec_uchar16
spu_splats (unsigned char a)
{
  return __builtin_spu_splats_0 (a);
}
static inline vec_char16
spu_splats (signed char a)
{
  return __builtin_spu_splats_1 (a);
}
static inline vec_char16
spu_splats (char a)
{
  return __builtin_spu_splats_1 (a);
}
static inline vec_ushort8
spu_splats (unsigned short a)
{
  return __builtin_spu_splats_2 (a);
}
static inline vec_short8
spu_splats (short a)
{
  return __builtin_spu_splats_3 (a);
}
static inline vec_uint4
spu_splats (unsigned int a)
{
  return __builtin_spu_splats_4 (a);
}
static inline vec_int4
spu_splats (int a)
{
  return __builtin_spu_splats_5 (a);
}
static inline vec_ullong2
spu_splats (unsigned long long a)
{
  return __builtin_spu_splats_6 (a);
}
static inline vec_llong2
spu_splats (long long a)
{
  return __builtin_spu_splats_7 (a);
}
static inline vec_float4
spu_splats (float a)
{
  return __builtin_spu_splats_8 (a);
}
static inline vec_double2
spu_splats (double a)
{
  return __builtin_spu_splats_9 (a);
}
static inline unsigned char
spu_extract (vec_uchar16 a, int b)
{
  return __builtin_spu_extract_0 (a, b);
}
static inline signed char
spu_extract (vec_char16 a, int b)
{
  return __builtin_spu_extract_1 (a, b);
}
static inline unsigned short
spu_extract (vec_ushort8 a, int b)
{
  return __builtin_spu_extract_2 (a, b);
}
static inline short
spu_extract (vec_short8 a, int b)
{
  return __builtin_spu_extract_3 (a, b);
}
static inline unsigned int
spu_extract (vec_uint4 a, int b)
{
  return __builtin_spu_extract_4 (a, b);
}
static inline int
spu_extract (vec_int4 a, int b)
{
  return __builtin_spu_extract_5 (a, b);
}
static inline unsigned long long
spu_extract (vec_ullong2 a, int b)
{
  return __builtin_spu_extract_6 (a, b);
}
static inline long long
spu_extract (vec_llong2 a, int b)
{
  return __builtin_spu_extract_7 (a, b);
}
static inline float
spu_extract (vec_float4 a, int b)
{
  return __builtin_spu_extract_8 (a, b);
}
static inline double
spu_extract (vec_double2 a, int b)
{
  return __builtin_spu_extract_9 (a, b);
}
static inline vec_uchar16
spu_insert (unsigned char a, vec_uchar16 b, int c)
{
  return __builtin_spu_insert_0 (a, b, c);
}
static inline vec_char16
spu_insert (signed char a, vec_char16 b, int c)
{
  return __builtin_spu_insert_1 (a, b, c);
}
static inline vec_ushort8
spu_insert (unsigned short a, vec_ushort8 b, int c)
{
  return __builtin_spu_insert_2 (a, b, c);
}
static inline vec_short8
spu_insert (short a, vec_short8 b, int c)
{
  return __builtin_spu_insert_3 (a, b, c);
}
static inline vec_uint4
spu_insert (unsigned int a, vec_uint4 b, int c)
{
  return __builtin_spu_insert_4 (a, b, c);
}
static inline vec_int4
spu_insert (int a, vec_int4 b, int c)
{
  return __builtin_spu_insert_5 (a, b, c);
}
static inline vec_ullong2
spu_insert (unsigned long long a, vec_ullong2 b, int c)
{
  return __builtin_spu_insert_6 (a, b, c);
}
static inline vec_llong2
spu_insert (long long a, vec_llong2 b, int c)
{
  return __builtin_spu_insert_7 (a, b, c);
}
static inline vec_float4
spu_insert (float a, vec_float4 b, int c)
{
  return __builtin_spu_insert_8 (a, b, c);
}
static inline vec_double2
spu_insert (double a, vec_double2 b, int c)
{
  return __builtin_spu_insert_9 (a, b, c);
}
static inline vec_uchar16
spu_promote (unsigned char a, int b)
{
  return __builtin_spu_promote_0 (a, b);
}
static inline vec_char16
spu_promote (signed char a, int b)
{
  return __builtin_spu_promote_1 (a, b);
}
static inline vec_char16
spu_promote (char a, int b)
{
  return __builtin_spu_promote_1 (a, b);
}
static inline vec_ushort8
spu_promote (unsigned short a, int b)
{
  return __builtin_spu_promote_2 (a, b);
}
static inline vec_short8
spu_promote (short a, int b)
{
  return __builtin_spu_promote_3 (a, b);
}
static inline vec_uint4
spu_promote (unsigned int a, int b)
{
  return __builtin_spu_promote_4 (a, b);
}
static inline vec_int4
spu_promote (int a, int b)
{
  return __builtin_spu_promote_5 (a, b);
}
static inline vec_ullong2
spu_promote (unsigned long long a, int b)
{
  return __builtin_spu_promote_6 (a, b);
}
static inline vec_llong2
spu_promote (long long a, int b)
{
  return __builtin_spu_promote_7 (a, b);
}
static inline vec_float4
spu_promote (float a, int b)
{
  return __builtin_spu_promote_8 (a, b);
}
static inline vec_double2
spu_promote (double a, int b)
{
  return __builtin_spu_promote_9 (a, b);
}
#endif  /* __cplusplus */

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

