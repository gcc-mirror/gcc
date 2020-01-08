; -*- buffer-read-only: t -*-
; Generated automatically by parsecpu.awk from arm-cpus.in.
; Do not edit.

; Copyright (C) 2011-2020 Free Software Foundation, Inc.

; This file is part of GCC.

; GCC is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3,
; or (at your option) any later version.

; GCC is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public
; License along with GCC; see the file COPYING3.  If not see
; <http://www.gnu.org/licenses/>.

(define_attr "tune"
	"arm8,arm810,strongarm,
	fa526,fa626,arm7tdmi,
	arm710t,arm9,arm9tdmi,
	arm920t,arm10tdmi,arm9e,
	arm10e,xscale,iwmmxt,
	iwmmxt2,fa606te,fa626te,
	fmp626,fa726te,arm926ejs,
	arm1026ejs,arm1136js,arm1136jfs,
	arm1176jzs,arm1176jzfs,mpcorenovfp,
	mpcore,arm1156t2s,arm1156t2fs,
	cortexm1,cortexm0,cortexm0plus,
	cortexm1smallmultiply,cortexm0smallmultiply,cortexm0plussmallmultiply,
	genericv7a,cortexa5,cortexa7,
	cortexa8,cortexa9,cortexa12,
	cortexa15,cortexa17,cortexr4,
	cortexr4f,cortexr5,cortexr7,
	cortexr8,cortexm7,cortexm4,
	cortexm3,marvell_pj4,cortexa15cortexa7,
	cortexa17cortexa7,cortexa32,cortexa35,
	cortexa53,cortexa57,cortexa72,
	cortexa73,exynosm1,xgene1,
	cortexa57cortexa53,cortexa72cortexa53,cortexa73cortexa35,
	cortexa73cortexa53,cortexa55,cortexa75,
	cortexa76,cortexa76ae,cortexa77,
	neoversen1,cortexa75cortexa55,cortexa76cortexa55,
	cortexm23,cortexm33,cortexm35p,
	cortexr52"
	(const (symbol_ref "((enum attr_tune) arm_tune)")))
