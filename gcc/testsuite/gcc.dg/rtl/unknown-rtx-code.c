void __RTL test (void)
{
  (function "test"
    (insn-chain
      (not-a-valid-kind-of-insn 1 0 0) ;; { dg-error "unknown rtx code" }
    ) ;; insn-chain
  ) ;; function
}
