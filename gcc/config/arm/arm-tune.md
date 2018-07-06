; -*- buffer-read-only: t -*-
; Generated automatically by parsecpu.awk from arm-cpus.in.
; Do not edit.

; Copyright (C) 2011-2018 Free Software Foundation, Inc.

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
	strongarm110,strongarm1100,strongarm1110,
	fa526,fa626,arm7tdmi,
	arm7tdmis,arm710t,arm720t,
	arm740t,arm9,arm9tdmi,
	arm920,arm920t,arm922t,
	arm940t,ep9312,arm10tdmi,
	arm1020t,arm9e,arm946es,
	arm966es,arm968es,arm10e,
	arm1020e,arm1022e,xscale,
	iwmmxt,iwmmxt2,fa606te,
	fa626te,fmp626,fa726te,
	arm926ejs,arm1026ejs,arm1136js,
	arm1136jfs,arm1176jzs,arm1176jzfs,
	mpcorenovfp,mpcore,arm1156t2s,
	arm1156t2fs,cortexm1,cortexm0,
	cortexm0plus,cortexm1smallmultiply,cortexm0smallmultiply,
	cortexm0plussmallmultiply,genericv7a,cortexa5,
	cortexa7,cortexa8,cortexa9,
	cortexa12,cortexa15,cortexa17,
	cortexr4,cortexr4f,cortexr5,
	cortexr7,cortexr8,cortexm7,
	cortexm4,cortexm3,marvell_pj4,
	cortexa15cortexa7,cortexa17cortexa7,cortexa32,
	cortexa35,cortexa53,cortexa57,
	cortexa72,cortexa73,exynosm1,
	xgene1,cortexa57cortexa53,cortexa72cortexa53,
	cortexa73cortexa35,cortexa73cortexa53,cortexa55,
	cortexa75,cortexa75cortexa55,cortexm23,
	cortexm33,cortexr52"
	(const (symbol_ref "((enum attr_tune) arm_tune)")))
