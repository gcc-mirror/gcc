;; e500 SPE description
;; Copyright (C) 2002-2017 Free Software Foundation, Inc.
;; Contributed by Aldy Hernandez (aldy@quesejoda.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Modes using a 64-bit register.
(define_mode_iterator SPE64 [DF V4HI V2SF V1DI V2SI])

;; Likewise, but allow TFmode (two registers) as well.
(define_mode_iterator SPE64TF [DF V4HI V2SF V1DI V2SI TF])

;; DImode and TImode.
(define_mode_iterator DITI [DI TI])
