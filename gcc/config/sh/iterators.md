;; Iterator definitions for GCC SH machine description files.
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_mode_iterator QIHISIDI [QI HI SI DI])
(define_mode_iterator QIHISI [QI HI SI])
(define_mode_iterator QIHI [QI HI])
(define_mode_iterator HISI [HI SI])
(define_mode_iterator SIDI [SI DI])

;; Mode attributes that can be used as the instruction suffix for mode
;; variant instructions.
(define_mode_attr bw [(QI "b") (HI "w")])
(define_mode_attr bwl [(QI "b") (HI "w") (SI "l")])

;; Sign/zero-extension code iterator.
(define_code_iterator SZ_EXTEND [sign_extend zero_extend])

;; Mode attributes for mov.b and mov.w displacement constraints.
(define_mode_attr disp04 [(QI "K04") (HI "K05")])
(define_mode_attr disp12 [(QI "K12") (HI "K13")])

;; Return codes.
(define_code_iterator any_return [return simple_return])

;; Lowpart subreg byte position code attributes for big and little endian.
(define_mode_attr lowpart_be [(QI "3") (HI "2")])
(define_mode_attr lowpart_le [(QI "0") (HI "0")])

;; Signed minimum/maximum code iterator.
(define_code_iterator SMIN_SMAX [smin smax])
