# At -Og no pass records the global range information
# necessary to optimize the strnlen calls down to
# a constant.  The framework assumes that the test
# will never call strnlen when the optimizer is
# enabled.  So we filter out the -Og run here.

set torture_eval_before_compile {
  if {[string match {*-Og*} "$option"]} {
    continue
  }
}

return 0

