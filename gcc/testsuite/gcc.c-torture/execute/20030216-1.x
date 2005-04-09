# This test requires constant propagation of loads and stores to be
# enabled.  This is only guaranteed at -O2 and higher.  Do not run
# at -O1.

set torture_eval_before_compile {
  if {[string match {*-O1*} "$option"]} {
    continue
  }
}

return 0

