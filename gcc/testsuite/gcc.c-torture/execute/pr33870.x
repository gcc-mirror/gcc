# The test breaks because of wrong alias info for -O2 and -Os

set torture_eval_before_compile {
  if {[string match {*-O[2s]*} "$option"]} {
     set torture_execute_xfail "*-*-*"
  }
}

return 0
