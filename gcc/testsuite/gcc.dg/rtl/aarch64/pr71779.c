/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-fdump-rtl-cse1" } */

/* Dump taken from comment 2 of PR 71779, of
   "...the relevant memory access coming out of expand"
   hand-edited to the compact dump format.  */

int __RTL (startwith ("cse1")) test (int n)
{
(function "fragment"
  (param "n"
    (DECL_RTL (reg/v:SI <1> [ n ]))
    (DECL_RTL_INCOMING (reg:SI x0 [ n ]))
  ) ;; param "n"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)

;; MEM[(struct isl_obj *)&obj1] = &isl_obj_map_vtable;
(insn 1045 (set (reg:SI <480>)
        (high:SI (symbol_ref:SI ("isl_obj_map_vtable")
                    [flags 0xc0]
                    <var_decl 0x7fa0363ea240 isl_obj_map_vtable>)))
     "y.c":12702)
(insn 1046 (set (reg/f:SI <479>)
        (lo_sum:SI (reg:SI <480>)
            (symbol_ref:SI ("isl_obj_map_vtable")
               [flags 0xc0]
               <var_decl 0x7fa0363ea240 isl_obj_map_vtable>)))
     "y.c":12702
     (expr_list:REG_EQUAL (symbol_ref:SI ("isl_obj_map_vtable")
                             [flags 0xc0]
                             <var_decl 0x7fa0363ea240 isl_obj_map_vtable>)))
(insn 1047 (set (reg:DI <481>)
        (subreg:DI (reg/f:SI <479>) 0)) "y.c":12702)
(insn 1048 (set (zero_extract:DI (reg/v:DI <191> [ obj1D.17368 ])
            (const_int 32)
            (const_int 0))
        (reg:DI <481>)) "y.c":12702)
;; Extra insn, to avoid all of the above from being deleted by DCE
(insn 1049 (set (mem:DI (reg:DI <191>) [1 i+0 S4 A32])
                         (const_int 1)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}

/* TODO: scan the dump.  */
