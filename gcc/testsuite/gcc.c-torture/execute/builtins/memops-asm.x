# Different translation units may have different user name overrides
# and we do not preserve enough context to known which one we want.

set torture_eval_before_compile {
  if {[string match {*-flto*} "$option"]} {
    continue
  }
}

return 0
