/* PowerPC E500 user include file.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldyh@redhat.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

#ifndef _SPE_H
#define _SPE_H

#define __vector __attribute__((vector_size(8)))

typedef int 	 		int32_t;
typedef unsigned 		uint32_t;
typedef short    		int16_t;
typedef unsigned short  	uint16_t;
typedef long long 		int64_t;
typedef unsigned long long	uint64_t;

typedef short 			__vector __ev64_s16__;
typedef unsigned short  	__vector __ev64_u16__;
typedef int 			__vector __ev64_s32__;
typedef unsigned 		__vector __ev64_u32__;
typedef long long 		__vector __ev64_s64__;
typedef unsigned long long 	__vector __ev64_u64__;
typedef float 			__vector __ev64_fs__;
typedef int 			__vector __ev64_opaque__;

#define __v2si __ev64_opaque__
#define __v2sf __ev64_fs__

#define __ev_addw(a,b) __builtin_spe_evaddw((__v2si) (a), (__v2si) (b))
#define __ev_addiw(a,b) __builtin_spe_evaddiw ((__v2si) (a), (b))
#define __ev_subfw(a,b) __builtin_spe_evsubfw ((__v2si) (a), (__v2si) (b))
#define __ev_subw(a,b) __builtin_spe_evsubfw ((__v2si) (b), (__v2si) (a))
/* ??? The spe_evsubifw pattern accepts operands reversed, so we need to also
   reverse them here between the intrinsic and the builtin function.  */
#define __ev_subifw(a,b) __builtin_spe_evsubifw ((__v2si) (b), (a))
#define __ev_subiw(a,b) __builtin_spe_evsubifw ((__v2si) (a), (b))
#define __ev_abs(a) __builtin_spe_evabs ((__v2si) (a))
#define __ev_neg(a) __builtin_spe_evneg ((__v2si) (a))
#define __ev_extsb(a) __builtin_spe_evextsb ((__v2si) (a))
#define __ev_extsh(a) __builtin_spe_evextsh ((__v2si) (a))
#define __ev_and(a,b) __builtin_spe_evand ((__v2si) (a), (__v2si) (b))
#define __ev_or(a,b) __builtin_spe_evor ((__v2si) (a), (__v2si) (b))
#define __ev_xor(a,b) __builtin_spe_evxor ((__v2si) (a), (__v2si) (b))
#define __ev_nand(a,b) __builtin_spe_evnand ((__v2si) (a), (__v2si) (b))
#define __ev_nor(a,b) __builtin_spe_evnor ((__v2si) (a), (__v2si) (b))
#define __ev_eqv(a,b) __builtin_spe_eveqv ((__v2si) (a), (__v2si) (b))
#define __ev_andc(a,b) __builtin_spe_evandc ((__v2si) (a), (__v2si) (b))
#define __ev_orc(a,b) __builtin_spe_evorc ((__v2si) (a), (__v2si) (b))
#define __ev_rlw(a,b) __builtin_spe_evrlw ((__v2si) (a), (__v2si) (b))
#define __ev_rlwi(a,b) __builtin_spe_evrlwi ((__v2si) (a), (b))
#define __ev_slw(a,b) __builtin_spe_evslw ((__v2si) (a), (__v2si) (b))
#define __ev_slwi(a,b) __builtin_spe_evslwi ((__v2si) (a), (b))
#define __ev_srws(a,b) __builtin_spe_evsrws ((__v2si) (a), (__v2si) (b))
#define __ev_srwu(a,b) __builtin_spe_evsrwu ((__v2si) (a), (__v2si) (b))
#define __ev_srwis(a,b) __builtin_spe_evsrwis ((__v2si) (a), (b))
#define __ev_srwiu(a,b) __builtin_spe_evsrwiu ((__v2si) (a), (b))
#define __ev_cntlzw(a) __builtin_spe_evcntlzw ((__v2si) (a))
#define __ev_cntlsw(a) __builtin_spe_evcntlsw ((__v2si) (a))
#define __ev_rndw(a) __builtin_spe_evrndw ((__v2si) (a))
#define __ev_mergehi(a,b) __builtin_spe_evmergehi ((__v2si) (a), (__v2si) (b))
#define __ev_mergelo(a,b) __builtin_spe_evmergelo ((__v2si) (a), (__v2si) (b))
#define __ev_mergelohi(a,b) __builtin_spe_evmergelohi ((__v2si) (a), (__v2si) (b))
#define __ev_mergehilo(a,b) __builtin_spe_evmergehilo ((__v2si) (a), (__v2si) (b))
#define __ev_splati(a) __builtin_spe_evsplati ((a))
#define __ev_splatfi(a) __builtin_spe_evsplatfi ((a))
#define __ev_divws(a,b) __builtin_spe_evdivws ((__v2si) (a), (__v2si) (b))
#define __ev_divwu(a,b) __builtin_spe_evdivwu ((__v2si) (a), (__v2si) (b))
#define __ev_mra(a) __builtin_spe_evmra ((__v2si) (a))

#define __brinc __builtin_spe_brinc

/* Loads.  */

#define __ev_lddx(a,b) __builtin_spe_evlddx ((void *)(a), (b))
#define __ev_ldwx(a,b) __builtin_spe_evldwx ((void *)(a), (b))
#define __ev_ldhx(a,b) __builtin_spe_evldhx ((void *)(a), (b))
#define __ev_lwhex(a,b) __builtin_spe_evlwhex ((a), (b))
#define __ev_lwhoux(a,b) __builtin_spe_evlwhoux ((a), (b))
#define __ev_lwhosx(a,b) __builtin_spe_evlwhosx ((a), (b))
#define __ev_lwwsplatx(a,b) __builtin_spe_evlwwsplatx ((a), (b))
#define __ev_lwhsplatx(a,b) __builtin_spe_evlwhsplatx ((a), (b))
#define __ev_lhhesplatx(a,b) __builtin_spe_evlhhesplatx ((a), (b))
#define __ev_lhhousplatx(a,b) __builtin_spe_evlhhousplatx ((a), (b))
#define __ev_lhhossplatx(a,b) __builtin_spe_evlhhossplatx ((a), (b))
#define __ev_ldd(a,b) __builtin_spe_evldd ((void *)(a), (b))
#define __ev_ldw(a,b) __builtin_spe_evldw ((void *)(a), (b))
#define __ev_ldh(a,b) __builtin_spe_evldh ((void *)(a), (b))
#define __ev_lwhe(a,b) __builtin_spe_evlwhe ((a), (b))
#define __ev_lwhou(a,b) __builtin_spe_evlwhou ((a), (b))
#define __ev_lwhos(a,b) __builtin_spe_evlwhos ((a), (b))
#define __ev_lwwsplat(a,b) __builtin_spe_evlwwsplat ((a), (b))
#define __ev_lwhsplat(a,b) __builtin_spe_evlwhsplat ((a), (b))
#define __ev_lhhesplat(a,b) __builtin_spe_evlhhesplat ((a), (b))
#define __ev_lhhousplat(a,b) __builtin_spe_evlhhousplat ((a), (b))
#define __ev_lhhossplat(a,b) __builtin_spe_evlhhossplat ((a), (b))

/* Stores.  */

#define __ev_stddx(a,b,c) __builtin_spe_evstddx ((__v2si)(a), (void *)(b), (c))
#define __ev_stdwx(a,b,c) __builtin_spe_evstdwx ((__v2si)(a), (void *)(b), (c))
#define __ev_stdhx(a,b,c) __builtin_spe_evstdhx ((__v2si)(a), (void *)(b), (c))
#define __ev_stwwex(a,b,c) __builtin_spe_evstwwex ((__v2si)(a), (b), (c))
#define __ev_stwwox(a,b,c) __builtin_spe_evstwwox ((__v2si)(a), (b), (c))
#define __ev_stwhex(a,b,c) __builtin_spe_evstwhex ((__v2si)(a), (b), (c))
#define __ev_stwhox(a,b,c) __builtin_spe_evstwhox ((__v2si)(a), (b), (c))
#define __ev_stdd(a,b,c) __builtin_spe_evstdd ((__v2si)(a), (void *)(b), (c))
#define __ev_stdw(a,b,c) __builtin_spe_evstdw ((__v2si)(a), (void *)(b), (c))
#define __ev_stdh(a,b,c) __builtin_spe_evstdh ((__v2si)(a), (void *)(b), (c))
#define __ev_stwwe(a,b,c) __builtin_spe_evstwwe ((__v2si)(a), (b), (c))
#define __ev_stwwo(a,b,c) __builtin_spe_evstwwo ((__v2si)(a), (b), (c))
#define __ev_stwhe(a,b,c) __builtin_spe_evstwhe ((__v2si)(a), (b), (c))
#define __ev_stwho(a,b,c) __builtin_spe_evstwho ((__v2si)(a), (b), (c))

/* Fixed point complex.  */

#define __ev_mhossf(a, b) __builtin_spe_evmhossf ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmf(a, b) __builtin_spe_evmhosmf ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmi(a, b) __builtin_spe_evmhosmi ((__v2si) (a), (__v2si) (b))
#define __ev_mhoumi(a, b) __builtin_spe_evmhoumi ((__v2si) (a), (__v2si) (b))
#define __ev_mhessf(a, b) __builtin_spe_evmhessf ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmf(a, b) __builtin_spe_evmhesmf ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmi(a, b) __builtin_spe_evmhesmi ((__v2si) (a), (__v2si) (b))
#define __ev_mheumi(a, b) __builtin_spe_evmheumi ((__v2si) (a), (__v2si) (b))
#define __ev_mhossfa(a, b) __builtin_spe_evmhossfa ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmfa(a, b) __builtin_spe_evmhosmfa ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmia(a, b) __builtin_spe_evmhosmia ((__v2si) (a), (__v2si) (b))
#define __ev_mhoumia(a, b) __builtin_spe_evmhoumia ((__v2si) (a), (__v2si) (b))
#define __ev_mhessfa(a, b) __builtin_spe_evmhessfa ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmfa(a, b) __builtin_spe_evmhesmfa ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmia(a, b) __builtin_spe_evmhesmia ((__v2si) (a), (__v2si) (b))
#define __ev_mheumia(a, b) __builtin_spe_evmheumia ((__v2si) (a), (__v2si) (b))

#define __ev_mhoumf __ev_mhoumi
#define __ev_mheumf __ev_mheumi
#define __ev_mhoumfa __ev_mhoumia
#define __ev_mheumfa __ev_mheumia

#define __ev_mhossfaaw(a, b) __builtin_spe_evmhossfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhossiaaw(a, b) __builtin_spe_evmhossiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmfaaw(a, b) __builtin_spe_evmhosmfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmiaaw(a, b) __builtin_spe_evmhosmiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhousiaaw(a, b) __builtin_spe_evmhousiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhoumiaaw(a, b) __builtin_spe_evmhoumiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhessfaaw(a, b) __builtin_spe_evmhessfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhessiaaw(a, b) __builtin_spe_evmhessiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmfaaw(a, b) __builtin_spe_evmhesmfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmiaaw(a, b) __builtin_spe_evmhesmiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mheusiaaw(a, b) __builtin_spe_evmheusiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mheumiaaw(a, b) __builtin_spe_evmheumiaaw ((__v2si) (a), (__v2si) (b))

#define __ev_mhousfaaw __ev_mhousiaaw
#define __ev_mhoumfaaw __ev_mhoumiaaw
#define __ev_mheusfaaw __ev_mheusiaaw
#define __ev_mheumfaaw __ev_mheumiaaw

#define __ev_mhossfanw(a, b) __builtin_spe_evmhossfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mhossianw(a, b) __builtin_spe_evmhossianw ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmfanw(a, b) __builtin_spe_evmhosmfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mhosmianw(a, b) __builtin_spe_evmhosmianw ((__v2si) (a), (__v2si) (b))
#define __ev_mhousianw(a, b) __builtin_spe_evmhousianw ((__v2si) (a), (__v2si) (b))
#define __ev_mhoumianw(a, b) __builtin_spe_evmhoumianw ((__v2si) (a), (__v2si) (b))
#define __ev_mhessfanw(a, b) __builtin_spe_evmhessfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mhessianw(a, b) __builtin_spe_evmhessianw ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmfanw(a, b) __builtin_spe_evmhesmfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mhesmianw(a, b) __builtin_spe_evmhesmianw ((__v2si) (a), (__v2si) (b))
#define __ev_mheusianw(a, b) __builtin_spe_evmheusianw ((__v2si) (a), (__v2si) (b))
#define __ev_mheumianw(a, b) __builtin_spe_evmheumianw ((__v2si) (a), (__v2si) (b))

#define __ev_mhousfanw __ev_mhousianw
#define __ev_mhoumfanw __ev_mhoumianw
#define __ev_mheusfanw __ev_mheusianw
#define __ev_mheumfanw __ev_mheumianw

#define __ev_mhogsmfaa(a, b) __builtin_spe_evmhogsmfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mhogsmiaa(a, b) __builtin_spe_evmhogsmiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mhogumiaa(a, b) __builtin_spe_evmhogumiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mhegsmfaa(a, b) __builtin_spe_evmhegsmfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mhegsmiaa(a, b) __builtin_spe_evmhegsmiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mhegumiaa(a, b) __builtin_spe_evmhegumiaa ((__v2si) (a), (__v2si) (b))

#define __ev_mhogumfaa __ev_mhogumiaa
#define __ev_mhegumfaa __ev_mhegumiaa

#define __ev_mhogsmfan(a, b) __builtin_spe_evmhogsmfan ((__v2si) (a), (__v2si) (b))
#define __ev_mhogsmian(a, b) __builtin_spe_evmhogsmian ((__v2si) (a), (__v2si) (b))
#define __ev_mhogumian(a, b) __builtin_spe_evmhogumian ((__v2si) (a), (__v2si) (b))
#define __ev_mhegsmfan(a, b) __builtin_spe_evmhegsmfan ((__v2si) (a), (__v2si) (b))
#define __ev_mhegsmian(a, b) __builtin_spe_evmhegsmian ((__v2si) (a), (__v2si) (b))
#define __ev_mhegumian(a, b) __builtin_spe_evmhegumian ((__v2si) (a), (__v2si) (b))

#define __ev_mhogumfan __ev_mhogumian
#define __ev_mhegumfan __ev_mhegumian

#define __ev_mwhssf(a, b) __builtin_spe_evmwhssf ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmf(a, b) __builtin_spe_evmwhsmf ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmi(a, b) __builtin_spe_evmwhsmi ((__v2si) (a), (__v2si) (b))
#define __ev_mwhumi(a, b) __builtin_spe_evmwhumi ((__v2si) (a), (__v2si) (b))
#define __ev_mwhssfa(a, b) __builtin_spe_evmwhssfa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmfa(a, b) __builtin_spe_evmwhsmfa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmia(a, b) __builtin_spe_evmwhsmia ((__v2si) (a), (__v2si) (b))
#define __ev_mwhumia(a, b) __builtin_spe_evmwhumia ((__v2si) (a), (__v2si) (b))

#define __ev_mwhumf __ev_mwhumi
#define __ev_mwhumfa __ev_mwhumia

#define __ev_mwlumi(a, b) __builtin_spe_evmwlumi ((__v2si) (a), (__v2si) (b))
#define __ev_mwlumia(a, b) __builtin_spe_evmwlumia ((__v2si) (a), (__v2si) (b))
#define __ev_mwlumiaaw(a, b) __builtin_spe_evmwlumiaaw ((__v2si) (a), (__v2si) (b))

#define __ev_mwlssiaaw(a, b) __builtin_spe_evmwlssiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlsmiaaw(a, b) __builtin_spe_evmwlsmiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlusiaaw(a, b) __builtin_spe_evmwlusiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlusiaaw(a, b) __builtin_spe_evmwlusiaaw ((__v2si) (a), (__v2si) (b))

#define __ev_mwlssianw(a, b) __builtin_spe_evmwlssianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlsmianw(a, b) __builtin_spe_evmwlsmianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlusianw(a, b) __builtin_spe_evmwlusianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwlumianw(a, b) __builtin_spe_evmwlumianw ((__v2si) (a), (__v2si) (b))

#define __ev_mwssf(a, b) __builtin_spe_evmwssf ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmf(a, b) __builtin_spe_evmwsmf ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmi(a, b) __builtin_spe_evmwsmi ((__v2si) (a), (__v2si) (b))
#define __ev_mwumi(a, b) __builtin_spe_evmwumi ((__v2si) (a), (__v2si) (b))
#define __ev_mwssfa(a, b) __builtin_spe_evmwssfa ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmfa(a, b) __builtin_spe_evmwsmfa ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmia(a, b) __builtin_spe_evmwsmia ((__v2si) (a), (__v2si) (b))
#define __ev_mwumia(a, b) __builtin_spe_evmwumia ((__v2si) (a), (__v2si) (b))

#define __ev_mwumf __ev_mwumi
#define __ev_mwumfa __ev_mwumia

#define __ev_mwssfaa(a, b) __builtin_spe_evmwssfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmfaa(a, b) __builtin_spe_evmwsmfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmiaa(a, b) __builtin_spe_evmwsmiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwumiaa(a, b) __builtin_spe_evmwumiaa ((__v2si) (a), (__v2si) (b))

#define __ev_mwumfaa __ev_mwumiaa

#define __ev_mwssfan(a, b) __builtin_spe_evmwssfan ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmfan(a, b) __builtin_spe_evmwsmfan ((__v2si) (a), (__v2si) (b))
#define __ev_mwsmian(a, b) __builtin_spe_evmwsmian ((__v2si) (a), (__v2si) (b))
#define __ev_mwumian(a, b) __builtin_spe_evmwumian ((__v2si) (a), (__v2si) (b))

#define __ev_mwumfan __ev_mwumian

#define __ev_addssiaaw(a) __builtin_spe_evaddssiaaw ((__v2si) (a))
#define __ev_addsmiaaw(a) __builtin_spe_evaddsmiaaw ((__v2si) (a))
#define __ev_addusiaaw(a) __builtin_spe_evaddusiaaw ((__v2si) (a))
#define __ev_addumiaaw(a) __builtin_spe_evaddumiaaw ((__v2si) (a))

#define __ev_addusfaaw __ev_addusiaaw
#define __ev_addumfaaw __ev_addumiaaw
#define __ev_addsmfaaw __ev_addsmiaaw
#define __ev_addssfaaw __ev_addssiaaw

#define __ev_subfssiaaw(a) __builtin_spe_evsubfssiaaw ((__v2si) (a))
#define __ev_subfsmiaaw(a) __builtin_spe_evsubfsmiaaw ((__v2si) (a))
#define __ev_subfusiaaw(a) __builtin_spe_evsubfusiaaw ((__v2si) (a))
#define __ev_subfumiaaw(a) __builtin_spe_evsubfumiaaw ((__v2si) (a))

#define __ev_subfusfaaw __ev_subfusiaaw
#define __ev_subfumfaaw __ev_subfumiaaw
#define __ev_subfsmfaaw __ev_subfsmiaaw
#define __ev_subfssfaaw __ev_subfssiaaw

/* Floating Point SIMD Instructions  */

/* These all return V2SF, but we need to cast them to V2SI
   because the SPE expect all functions to be __ev64_opaque__.  */

#define __ev_fsabs(a) ((__v2si) __builtin_spe_evfsabs ((__v2sf) (a)))
#define __ev_fsnabs(a) ((__v2si) __builtin_spe_evfsnabs ((__v2sf) (a)))
#define __ev_fsneg(a) ((__v2si) __builtin_spe_evfsneg ((__v2sf) (a)))
#define __ev_fsadd(a, b) ((__v2si) __builtin_spe_evfsadd ((__v2sf) (a), (__v2sf) (b)))
#define __ev_fssub(a, b) ((__v2si) __builtin_spe_evfssub ((__v2sf) (a), (__v2sf) (b)))
#define __ev_fsmul(a, b) ((__v2si) __builtin_spe_evfsmul ((__v2sf) (a), (__v2sf) b))
#define __ev_fsdiv(a, b) ((__v2si) __builtin_spe_evfsdiv ((__v2sf) (a), (__v2sf) b))
#define __ev_fscfui(a) ((__v2si) __builtin_spe_evfscfui ((__v2si) (a)))
#define __ev_fscfsi(a) ((__v2si) __builtin_spe_evfscfsi ((__v2sf) (a)))
#define __ev_fscfuf(a) ((__v2si) __builtin_spe_evfscfuf ((__v2sf) (a)))
#define __ev_fscfsf(a) ((__v2si) __builtin_spe_evfscfsf ((__v2sf) (a)))
#define __ev_fsctui(a) ((__v2si) __builtin_spe_evfsctui ((__v2sf) (a)))
#define __ev_fsctsi(a) ((__v2si) __builtin_spe_evfsctsi ((__v2sf) (a)))
#define __ev_fsctuf(a) ((__v2si) __builtin_spe_evfsctuf ((__v2sf) (a)))
#define __ev_fsctsf(a) ((__v2si) __builtin_spe_evfsctsf ((__v2sf) (a)))
#define __ev_fsctuiz(a) ((__v2si) __builtin_spe_evfsctuiz ((__v2sf) (a)))
#define __ev_fsctsiz(a) ((__v2si) __builtin_spe_evfsctsiz ((__v2sf) (a)))

/* NOT SUPPORTED IN FIRST e500, support via two instructions:  */

#define __ev_mwhusfaaw  __ev_mwhusiaaw
#define __ev_mwhumfaaw  __ev_mwhumiaaw
#define __ev_mwhusfanw  __ev_mwhusianw
#define __ev_mwhumfanw  __ev_mwhumianw
#define __ev_mwhgumfaa  __ev_mwhgumiaa
#define __ev_mwhgumfan  __ev_mwhgumian

#define __ev_mwhgssfaa(a, b) __internal_ev_mwhgssfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgsmfaa(a, b) __internal_ev_mwhgsmfaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgsmiaa(a, b) __internal_ev_mwhgsmiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgumiaa(a, b) __internal_ev_mwhgumiaa ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgssfan(a, b) __internal_ev_mwhgssfan ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgsmfan(a, b) __internal_ev_mwhgsmfan ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgsmian(a, b) __internal_ev_mwhgsmian ((__v2si) (a), (__v2si) (b))
#define __ev_mwhgumian(a, b) __internal_ev_mwhgumian ((__v2si) (a), (__v2si) (b))
#define __ev_mwhssiaaw(a, b) __internal_ev_mwhssiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhssfaaw(a, b) __internal_ev_mwhssfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmfaaw(a, b) __internal_ev_mwhsmfaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmiaaw(a, b) __internal_ev_mwhsmiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhusiaaw(a, b) __internal_ev_mwhusiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhumiaaw(a, b) __internal_ev_mwhumiaaw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhssfanw(a, b) __internal_ev_mwhssfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhssianw(a, b) __internal_ev_mwhssianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmfanw(a, b) __internal_ev_mwhsmfanw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhsmianw(a, b) __internal_ev_mwhsmianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhusianw(a, b) __internal_ev_mwhusianw ((__v2si) (a), (__v2si) (b))
#define __ev_mwhumianw(a, b) __internal_ev_mwhumianw ((__v2si) (a), (__v2si) (b))

static inline __ev64_opaque__
__internal_ev_mwhssfaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhssf (a, b);
  return __ev_addssiaaw (t);
}

static inline __ev64_opaque__
__internal_ev_mwhssiaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;
  
  t = __ev_mwhsmi (a, b);
  return __ev_addssiaaw (t);
}

static inline __ev64_opaque__
__internal_ev_mwhsmfaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmf (a, b);
  return __ev_addsmiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhsmiaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmi (a, b);
  return __ev_addsmiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhusiaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_addusiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhumiaaw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_addumiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhssfanw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhssf (a, b);
  return __ev_subfssiaaw (t);
}

static inline __ev64_opaque__
__internal_ev_mwhssianw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmi (a, b);
  return __ev_subfssiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhsmfanw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmf (a, b);
  return __ev_subfsmiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhsmianw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmi (a, b);
  return __ev_subfsmiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhusianw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_subfusiaaw (t);
}
 
static inline __ev64_opaque__
__internal_ev_mwhumianw (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_subfumiaaw (t);
}

static inline __ev64_opaque__
__internal_ev_mwhgssfaa (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhssf (a, b);
  return __ev_mwsmiaa (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgsmfaa (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmf (a, b);
  return __ev_mwsmiaa (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgsmiaa (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmi (a, b);
  return __ev_mwsmiaa (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgumiaa (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_mwumiaa (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgssfan (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhssf (a, b);
  return __ev_mwsmian (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgsmfan (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmf (a, b);
  return __ev_mwsmian (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgsmian (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhsmi (a, b);
  return __ev_mwsmian (t, ((__ev64_s32__){1, 1}));
}

static inline __ev64_opaque__
__internal_ev_mwhgumian (__ev64_opaque__ a, __ev64_opaque__ b)
{
  __ev64_opaque__ t;

  t = __ev_mwhumi (a, b);
  return __ev_mwumian (t, ((__ev64_s32__){1, 1}));
}

/* END OF NOT SUPPORTED */

/* __ev_create* functions.  */

#define __ev_create_ufix32_u32 __ev_create_u32
#define __ev_create_sfix32_s32 __ev_create_s32

static inline __ev64_opaque__
__ev_create_s16 (int16_t a, int16_t b, int16_t c, int16_t d)
{
  union
  {
    __ev64_opaque__ v;
    int16_t i[4];
  } u;

  u.i[0] = a;
  u.i[1] = b;
  u.i[2] = c;
  u.i[3] = d;

  return u.v;
}

static inline __ev64_opaque__
__ev_create_u16 (uint16_t a, uint16_t b, uint16_t c, uint16_t d)
				  
{
  union
  {
    __ev64_opaque__ v;
    uint16_t i[4];
  } u;

  u.i[0] = a;
  u.i[1] = b;
  u.i[2] = c;
  u.i[3] = d;

  return u.v;
}

static inline __ev64_opaque__
__ev_create_s32 (int32_t a, int32_t b)
{
  union
  {
    __ev64_opaque__ v;
   int32_t i[2];
  } u;

  u.i[0] = a;
  u.i[1] = b;

  return u.v;
}

static inline __ev64_opaque__
__ev_create_u32 (uint32_t a, uint32_t b)
{
  union
  {
    __ev64_opaque__ v;
    uint32_t i[2];
  } u;

  u.i[0] = a;
  u.i[1] = b;

  return u.v;
}

static inline __ev64_opaque__
__ev_create_fs (float a, float b)
{
  union
  {
    __ev64_opaque__ v;
    float f[2];
  } u;

  u.f[0] = a;
  u.f[1] = b;
  
  return u.v;
}

static inline __ev64_opaque__
__ev_create_sfix32_fs (float a, float b)
{
  __ev64_opaque__ ev;

  ev = (__ev64_opaque__) __ev_create_fs (a, b);
  return (__ev64_opaque__) __builtin_spe_evfsctsf ((__v2sf) ev);
}

static inline __ev64_opaque__
__ev_create_ufix32_fs (float a, float b)
{
  __ev64_opaque__ ev;

  ev = (__ev64_opaque__) __ev_create_fs (a, b);
  return (__ev64_opaque__) __builtin_spe_evfsctuf ((__v2sf) ev);
}

static inline __ev64_opaque__
__ev_create_s64 (int64_t a)
{
  union
  {
    __ev64_opaque__ v;
    int64_t i;
  } u;

  u.i = a;
  return u.v;
}

static inline __ev64_opaque__
__ev_create_u64 (uint64_t a)
{
  union
  {
    __ev64_opaque__ v;
    uint64_t i;
  } u;

  u.i = a;
  return u.v;
}

static inline uint64_t
__ev_convert_u64 (__ev64_opaque__ a)
{
  return (uint64_t) a;
}

static inline int64_t
__ev_convert_s64 (__ev64_opaque__ a)
{
  return (int64_t) a;
}

/* __ev_get_* functions.  */

#define __ev_get_upper_u32(a) __ev_get_u32_internal ((__ev64_opaque__) (a), 0)
#define __ev_get_lower_u32(a) __ev_get_u32_internal ((__ev64_opaque__) (a), 1)
#define __ev_get_upper_s32(a) __ev_get_s32_internal ((__ev64_opaque__) (a), 0)
#define __ev_get_lower_s32(a) __ev_get_s32_internal ((__ev64_opaque__) (a), 1)
#define __ev_get_upper_fs(a) __ev_get_fs_internal ((__ev64_opaque__) (a), 0)
#define __ev_get_lower_fs(a) __ev_get_fs_internal ((__ev64_opaque__) (a), 1)
#define __ev_get_upper_ufix32_u32(a) __ev_get_upper_u32(a)
#define __ev_get_lower_ufix32_u32(a) __ev_get_lower_u32(a)
#define __ev_get_upper_sfix32_s32(a) __ev_get_upper_s32(a)
#define __ev_get_lower_sfix32_s32(a) __ev_get_lower_s32(a)
#define __ev_get_upper_sfix32_fs(a)  __ev_get_sfix32_fs (a, 0)
#define __ev_get_lower_sfix32_fs(a)  __ev_get_sfix32_fs (a, 1)
#define __ev_get_upper_ufix32_fs(a)  __ev_get_ufix32_fs (a, 0)
#define __ev_get_lower_ufix32_fs(a)  __ev_get_ufix32_fs (a, 1)

#define __ev_get_u32(a, b) __ev_get_u32_internal ((__ev64_opaque__) (a), b)
#define __ev_get_s32(a, b) __ev_get_s32_internal ((__ev64_opaque__) (a), b)
#define __ev_get_fs(a, b) __ev_get_fs_internal ((__ev64_opaque__) (a), b)
#define __ev_get_u16(a, b) __ev_get_u16_internal ((__ev64_opaque__) (a), b)
#define __ev_get_s16(a, b) __ev_get_s16_internal ((__ev64_opaque__) (a), b)

#define __ev_get_ufix32_u32(a, b) __ev_get_u32 (a, b)
#define __ev_get_sfix32_s32(a, b) __ev_get_s32 (a, b)
#define __ev_get_ufix32_fs(a, b)     __ev_get_ufix32_fs_internal ((__ev64_opaque__)(a), b)
#define __ev_get_sfix32_fs(a, b)     __ev_get_sfix32_fs_internal ((__ev64_opaque__)(a), b)

static inline uint32_t
__ev_get_u32_internal (__ev64_opaque__ a, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    uint32_t i[2];
  } u;

  u.v = a;
  return u.i[pos];
}

static inline int32_t
__ev_get_s32_internal (__ev64_opaque__ a, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    int32_t i[2];
  } u;

  u.v = a;
  return u.i[pos];
}

static inline float
__ev_get_fs_internal (__ev64_opaque__ a, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    float f[2];
  } u;

  u.v = a;
  return u.f[pos];
}

static inline float
__ev_get_sfix32_fs_internal (__ev64_opaque__ a, uint32_t pos)
{
  __ev64_fs__ v;

  v = __builtin_spe_evfscfsf ((__v2sf) a);
  return __ev_get_fs_internal ((__ev64_opaque__) v, pos);
}

static inline float
__ev_get_ufix32_fs_internal (__ev64_opaque__ a, uint32_t pos)
{
  __ev64_fs__ v;

  v = __builtin_spe_evfscfuf ((__v2sf) a);
  return __ev_get_fs_internal ((__ev64_opaque__) v, pos);
}

static inline uint16_t
__ev_get_u16_internal (__ev64_opaque__ a, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    uint16_t i[4];
  } u;

  u.v = a;
  return u.i[pos];
}

static inline int16_t
__ev_get_s16_internal (__ev64_opaque__ a, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    int16_t i[4];
  } u;

  u.v = a;
  return u.i[pos];
}

/* __ev_set_* functions.  */

#define __ev_set_u32(a, b, c) __ev_set_u32_internal ((__ev64_opaque__) a, b, c)
#define __ev_set_s32(a, b, c) __ev_set_s32_internal ((__ev64_opaque__) a, b, c)
#define __ev_set_fs(a, b, c) __ev_set_fs_internal ((__ev64_opaque__) a, b, c)
#define __ev_set_u16(a, b, c) __ev_set_u16_internal ((__ev64_opaque__) a, b, c)
#define __ev_set_s16(a, b, c) __ev_set_s16_internal ((__ev64_opaque__) a, b, c)

#define __ev_set_ufix32_u32 __ev_set_u32
#define __ev_set_sfix32_s32 __ev_set_s32

#define __ev_set_sfix32_fs(a, b, c)  __ev_set_sfix32_fs_internal ((__ev64_opaque__) (a), b, c)
#define __ev_set_ufix32_fs(a, b, c)  __ev_set_ufix32_fs_internal ((__ev64_opaque__) (a), b, c)

#define __ev_set_upper_u32(a, b) __ev_set_u32 (a, b, 0)
#define __ev_set_lower_u32(a, b) __ev_set_u32 (a, b, 1)
#define __ev_set_upper_s32(a, b) __ev_set_s32 (a, b, 0)
#define __ev_set_lower_s32(a, b) __ev_set_s32 (a, b, 1)
#define __ev_set_upper_fs(a, b) __ev_set_fs (a, b, 0)
#define __ev_set_lower_fs(a, b) __ev_set_fs (a, b, 1)
#define __ev_set_upper_ufix32_u32 __ev_set_upper_u32
#define __ev_set_lower_ufix32_u32 __ev_set_lower_u32
#define __ev_set_upper_sfix32_s32 __ev_set_upper_s32
#define __ev_set_lower_sfix32_s32 __ev_set_lower_s32
#define __ev_set_upper_sfix32_fs(a, b)  __ev_set_sfix32_fs (a, b, 0)
#define __ev_set_lower_sfix32_fs(a, b)  __ev_set_sfix32_fs (a, b, 1)
#define __ev_set_upper_ufix32_fs(a, b)  __ev_set_ufix32_fs (a, b, 0)
#define __ev_set_lower_ufix32_fs(a, b)  __ev_set_ufix32_fs (a, b, 1)

#define __ev_set_acc_vec64(a) __builtin_spe_evmra ((__ev64_opaque__)(a))

static inline __ev64_opaque__
__ev_set_acc_u64 (uint64_t a)
{
  __ev64_opaque__ ev32;
  ev32 = __ev_create_u64 (a);
  __ev_mra (ev32);
  return ev32;
}

static inline __ev64_opaque__
__ev_set_acc_s64 (int64_t a)
{
  __ev64_opaque__ ev32;
  ev32 = __ev_create_s64 (a);
  __ev_mra (ev32);
  return ev32;
}

static inline __ev64_opaque__
__ev_set_u32_internal (__ev64_opaque__ a, uint32_t b, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    uint32_t i[2];
  } u;

  u.v = a;
  u.i[pos] = b;
  return u.v;
}

static inline __ev64_opaque__
__ev_set_s32_internal (__ev64_opaque__ a, int32_t b, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    int32_t i[2];
  } u;

  u.v = a;
  u.i[pos] = b;
  return u.v;
}

static inline __ev64_opaque__
__ev_set_fs_internal (__ev64_opaque__ a, float b, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    float f[2];
  } u;

  u.v = a;
  u.f[pos] = b;
  return u.v;
}

static inline __ev64_opaque__
__ev_set_sfix32_fs_internal (__ev64_opaque__ a, float b, uint32_t pos)
{
  __ev64_opaque__ v;
  float other;

  /* Get other half.  */
  other = __ev_get_fs_internal (a, pos ^ 1);

  /* Make an sfix32 with 'b'.  */
  v = __ev_create_sfix32_fs (b, b);

  /* Set other half to what it used to be.  */
  return __ev_set_fs_internal (v, other, pos ^ 1);
}

static inline __ev64_opaque__
__ev_set_ufix32_fs_internal (__ev64_opaque__ a, float b, uint32_t pos)
{
  __ev64_opaque__ v;
  float other;

  /* Get other half.  */
  other = __ev_get_fs_internal (a, pos ^ 1);

  /* Make an ufix32 with 'b'.  */
  v = __ev_create_ufix32_fs (b, b);

  /* Set other half to what it used to be.  */
  return __ev_set_fs_internal (v, other, pos ^ 1);
}

static inline __ev64_opaque__
__ev_set_u16_internal (__ev64_opaque__ a, uint16_t b, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    uint16_t i[4];
  } u;

  u.v = a;
  u.i[pos] = b;
  return u.v;
}

static inline __ev64_opaque__
__ev_set_s16_internal (__ev64_opaque__ a, int16_t b, uint32_t pos)
{
  union
  {
    __ev64_opaque__ v;
    int16_t i[4];
  } u;

  u.v = a;
  u.i[pos] = b;
  return u.v;
}

/* Predicates.  */

#define __pred_all	0
#define __pred_any	1
#define __pred_upper	2
#define __pred_lower	3

#define __ev_any_gts(a, b)		__builtin_spe_evcmpgts (__pred_any, (__v2si) (a), (__v2si) (b))
#define __ev_all_gts(a, b)		__builtin_spe_evcmpgts (__pred_all, (__v2si) (a), (__v2si) (b))
#define __ev_upper_gts(a, b)		__builtin_spe_evcmpgts (__pred_upper, (__v2si) (a), (__v2si) (b))
#define __ev_lower_gts(a, b)		__builtin_spe_evcmpgts (__pred_lower, (__v2si) (a), (__v2si) (b))
#define __ev_select_gts(a, b, c, d) 	((__v2si) __builtin_spe_evsel_gts ((__v2si) (a), (__v2si) (b), (__v2si) (c), (__v2si) (d)))

#define __ev_any_gtu(a, b)		__builtin_spe_evcmpgtu (__pred_any, (__v2si) (a), (__v2si) (b))
#define __ev_all_gtu(a, b)		__builtin_spe_evcmpgtu (__pred_all, (__v2si) (a), (__v2si) (b))
#define __ev_upper_gtu(a, b)		__builtin_spe_evcmpgtu (__pred_upper, (__v2si) (a), (__v2si) (b))
#define __ev_lower_gtu(a, b)		__builtin_spe_evcmpgtu (__pred_lower, (__v2si) (a), (__v2si) (b))
#define __ev_select_gtu(a, b, c, d) 	((__v2si) __builtin_spe_evsel_gtu ((__v2si) (a), (__v2si) (b), (__v2si) (c), (__v2si) (d)))

#define __ev_any_lts(a, b)		__builtin_spe_evcmplts (__pred_any, (__v2si) (a), (__v2si) (b))
#define __ev_all_lts(a, b)		__builtin_spe_evcmplts (__pred_all, (__v2si) (a), (__v2si) (b))
#define __ev_upper_lts(a, b)		__builtin_spe_evcmplts (__pred_upper, (__v2si) (a), (__v2si) (b))
#define __ev_lower_lts(a, b)		__builtin_spe_evcmplts (__pred_lower, (__v2si) (a), (__v2si) (b))
#define __ev_select_lts(a, b, c, d) 	((__v2si) __builtin_spe_evsel_lts ((__v2si) (a), (__v2si) (b), (__v2si) (c), (__v2si) (d)))

#define __ev_any_ltu(a, b)		__builtin_spe_evcmpltu (__pred_any, (__v2si) (a), (__v2si) (b))
#define __ev_all_ltu(a, b)		__builtin_spe_evcmpltu (__pred_all, (__v2si) (a), (__v2si) (b))
#define __ev_upper_ltu(a, b)		__builtin_spe_evcmpltu (__pred_upper, (__v2si) (a), (__v2si) (b))
#define __ev_lower_ltu(a, b)		__builtin_spe_evcmpltu (__pred_lower, (__v2si) (a), (__v2si) (b))
#define __ev_select_ltu(a, b, c, d) 	((__v2si) __builtin_spe_evsel_ltu ((__v2si) (a), (__v2si) (b), (__v2si) (c), (__v2si) (d)))
#define __ev_any_eq(a, b)		__builtin_spe_evcmpeq (__pred_any, (__v2si) (a), (__v2si) (b))
#define __ev_all_eq(a, b)		__builtin_spe_evcmpeq (__pred_all, (__v2si) (a), (__v2si) (b))
#define __ev_upper_eq(a, b)		__builtin_spe_evcmpeq (__pred_upper, (__v2si) (a), (__v2si) (b))
#define __ev_lower_eq(a, b)		__builtin_spe_evcmpeq (__pred_lower, (__v2si) (a), (__v2si) (b))
#define __ev_select_eq(a, b, c, d) 	((__v2si) __builtin_spe_evsel_eq ((__v2si) (a), (__v2si) (b), (__v2si) (c), (__v2si) (d)))

#define __ev_any_fs_gt(a, b)		__builtin_spe_evfscmpgt (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_gt(a, b)		__builtin_spe_evfscmpgt (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_gt(a, b)		__builtin_spe_evfscmpgt (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_gt(a, b)		__builtin_spe_evfscmpgt (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_gt(a, b, c, d)	((__v2si) __builtin_spe_evsel_fsgt ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

#define __ev_any_fs_lt(a, b)		__builtin_spe_evfscmplt (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_lt(a, b)		__builtin_spe_evfscmplt (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_lt(a, b)		__builtin_spe_evfscmplt (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_lt(a, b)		__builtin_spe_evfscmplt (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_lt(a, b, c, d)	((__v2si) __builtin_spe_evsel_fslt ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

#define __ev_any_fs_eq(a, b)		__builtin_spe_evfscmpeq (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_eq(a, b)		__builtin_spe_evfscmpeq (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_eq(a, b)		__builtin_spe_evfscmpeq (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_eq(a, b)		__builtin_spe_evfscmpeq (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_eq(a, b, c, d)	((__v2si) __builtin_spe_evsel_fseq ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

#define __ev_any_fs_tst_gt(a, b)	__builtin_spe_evfststgt (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_tst_gt(a, b)	__builtin_spe_evfststgt (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_tst_gt(a, b)	__builtin_spe_evfststgt (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_tst_gt(a, b)	__builtin_spe_evfststgt (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_tst_gt(a, b, c, d)	((__v2si) __builtin_spe_evsel_fststgt ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

#define __ev_any_fs_tst_lt(a, b)	__builtin_spe_evfststlt (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_tst_lt(a, b)	__builtin_spe_evfststlt (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_tst_lt(a, b)	__builtin_spe_evfststlt (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_tst_lt(a, b)	__builtin_spe_evfststlt (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_tst_lt(a, b, c, d)	((__v2si) __builtin_spe_evsel_fststlt ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

#define __ev_any_fs_tst_eq(a, b)	__builtin_spe_evfststeq (__pred_any, (__v2sf) (a), (__v2sf) (b))
#define __ev_all_fs_tst_eq(a, b)	__builtin_spe_evfststeq (__pred_all, (__v2sf) (a), (__v2sf) (b))
#define __ev_upper_fs_tst_eq(a, b)	__builtin_spe_evfststeq (__pred_upper, (__v2sf) (a), (__v2sf) (b))
#define __ev_lower_fs_tst_eq(a, b)	__builtin_spe_evfststeq (__pred_lower, (__v2sf) (a), (__v2sf) (b))
#define __ev_select_fs_tst_eq(a, b, c, d)	((__v2si) __builtin_spe_evsel_fststeq ((__v2sf) (a), (__v2sf) (b), (__v2sf) (c), (__v2sf) (d)))

/* SPEFSCR accesor functions.  */

#define __SPEFSCR_SOVH		0x80000000
#define __SPEFSCR_OVH		0x40000000
#define __SPEFSCR_FGH		0x20000000
#define __SPEFSCR_FXH		0x10000000
#define __SPEFSCR_FINVH		0x08000000
#define __SPEFSCR_FDBZH		0x04000000
#define __SPEFSCR_FUNFH		0x02000000
#define __SPEFSCR_FOVFH		0x01000000
/* 2 unused bits.  */
#define __SPEFSCR_FINXS		0x00200000
#define __SPEFSCR_FINVS		0x00100000
#define __SPEFSCR_FDBZS		0x00080000
#define __SPEFSCR_FUNFS		0x00040000
#define __SPEFSCR_FOVFS		0x00020000
#define __SPEFSCR_MODE		0x00010000
#define __SPEFSCR_SOV		0x00008000
#define __SPEFSCR_OV		0x00004000
#define __SPEFSCR_FG		0x00002000
#define __SPEFSCR_FX		0x00001000
#define __SPEFSCR_FINV		0x00000800
#define __SPEFSCR_FDBZ		0x00000400
#define __SPEFSCR_FUNF		0x00000200
#define __SPEFSCR_FOVF		0x00000100
/* 1 unused bit.  */
#define __SPEFSCR_FINXE		0x00000040
#define __SPEFSCR_FINVE		0x00000020
#define __SPEFSCR_FDBZE		0x00000010
#define __SPEFSCR_FUNFE		0x00000008
#define __SPEFSCR_FOVFE		0x00000004
#define __SPEFSCR_FRMC		0x00000003

#define __ev_get_spefscr_sovh() (__builtin_spe_mfspefscr () & __SPEFSCR_SOVH)
#define __ev_get_spefscr_ovh() (__builtin_spe_mfspefscr () & __SPEFSCR_OVH)
#define __ev_get_spefscr_fgh() (__builtin_spe_mfspefscr () & __SPEFSCR_FGH)
#define __ev_get_spefscr_fxh() (__builtin_spe_mfspefscr () & __SPEFSCR_FXH)
#define __ev_get_spefscr_finvh() (__builtin_spe_mfspefscr () & __SPEFSCR_FINVH)
#define __ev_get_spefscr_fdbzh() (__builtin_spe_mfspefscr () & __SPEFSCR_FDBZH)
#define __ev_get_spefscr_funfh() (__builtin_spe_mfspefscr () & __SPEFSCR_FUNFH)
#define __ev_get_spefscr_fovfh() (__builtin_spe_mfspefscr () & __SPEFSCR_FOVFH)
#define __ev_get_spefscr_finxs() (__builtin_spe_mfspefscr () & __SPEFSCR_FINXS)
#define __ev_get_spefscr_finvs() (__builtin_spe_mfspefscr () & __SPEFSCR_FINVS)
#define __ev_get_spefscr_fdbzs() (__builtin_spe_mfspefscr () & __SPEFSCR_FDBZS)
#define __ev_get_spefscr_funfs() (__builtin_spe_mfspefscr () & __SPEFSCR_FUNFS)
#define __ev_get_spefscr_fovfs() (__builtin_spe_mfspefscr () & __SPEFSCR_FOVFS)
#define __ev_get_spefscr_mode() (__builtin_spe_mfspefscr () & __SPEFSCR_MODE)
#define __ev_get_spefscr_sov() (__builtin_spe_mfspefscr () & __SPEFSCR_SOV)
#define __ev_get_spefscr_ov() (__builtin_spe_mfspefscr () & __SPEFSCR_OV)
#define __ev_get_spefscr_fg() (__builtin_spe_mfspefscr () & __SPEFSCR_FG)
#define __ev_get_spefscr_fx() (__builtin_spe_mfspefscr () & __SPEFSCR_FX)
#define __ev_get_spefscr_finv() (__builtin_spe_mfspefscr () & __SPEFSCR_FINV)
#define __ev_get_spefscr_fdbz() (__builtin_spe_mfspefscr () & __SPEFSCR_FDBZ)
#define __ev_get_spefscr_funf() (__builtin_spe_mfspefscr () & __SPEFSCR_FUNF)
#define __ev_get_spefscr_fovf() (__builtin_spe_mfspefscr () & __SPEFSCR_FOVF)
#define __ev_get_spefscr_finxe() (__builtin_spe_mfspefscr () & __SPEFSCR_FINXE)
#define __ev_get_spefscr_finve() (__builtin_spe_mfspefscr () & __SPEFSCR_FINVE)
#define __ev_get_spefscr_fdbze() (__builtin_spe_mfspefscr () & __SPEFSCR_FDBZE)
#define __ev_get_spefscr_funfe() (__builtin_spe_mfspefscr () & __SPEFSCR_FUNFE)
#define __ev_get_spefscr_fovfe() (__builtin_spe_mfspefscr () & __SPEFSCR_FOVFE)
#define __ev_get_spefscr_frmc() (__builtin_spe_mfspefscr () & __SPEFSCR_FRMC)

static inline void
__ev_clr_spefscr_field (int mask)
{
  int i;

  i = __builtin_spe_mfspefscr ();
  i &= ~mask;
  __builtin_spe_mtspefscr (i);
}

#define __ev_clr_spefscr_sovh() __ev_clr_spefscr_field (__SPEFSCR_SOVH)
#define __ev_clr_spefscr_sov() __ev_clr_spefscr_field (__SPEFSCR_SOV)
#define __ev_clr_spefscr_finxs() __ev_clr_spefscr_field (__SPEFSCR_FINXS)
#define __ev_clr_spefscr_finvs() __ev_clr_spefscr_field (__SPEFSCR_FINVS)
#define __ev_clr_spefscr_fdbzs() __ev_clr_spefscr_field (__SPEFSCR_FDBZS)
#define __ev_clr_spefscr_funfs() __ev_clr_spefscr_field (__SPEFSCR_FUNFS)
#define __ev_clr_spefscr_fovfs() __ev_clr_spefscr_field (__SPEFSCR_FOVFS)

/* Set rounding mode:
     rnd = 0 (nearest)
     rnd = 1 (zero)
     rnd = 2 (+inf)
     rnd = 3 (-inf).  */

static inline void
__ev_set_spefscr_frmc (int rnd)
{
  int i;

  i = __builtin_spe_mfspefscr ();
  i &= ~__SPEFSCR_FRMC;
  i |= rnd;
}

#endif /* _SPE_H */
