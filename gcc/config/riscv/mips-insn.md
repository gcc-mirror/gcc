;; Machine description for MIPS custom instructions.
;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_insn "*mov<GPR:mode><X:mode>cc_bitmanip"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(if_then_else:GPR (any_eq:X (match_operand:X 1 "register_operand" "r")
				    (match_operand:X 2 "const_0_operand" "J"))
			  (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			  (match_operand:GPR 4 "reg_or_0_operand" "rJ")))]
  "TARGET_XMIPSCMOV"
{
  enum rtx_code code = <CODE>;
  if (code == NE)
    return "mips.ccmov\t%0,%1,%z3,%z4";
  else
    return "mips.ccmov\t%0,%1,%z4,%z3";
}
[(set_attr "type" "condmove")
 (set_attr "mode" "<GPR:MODE>")])
