# This test relies on __builtin_return_address(1) returning something
# useful or NULL.  This is not guaranteed to be be so, especially when 
# -fomit-frame-pointer is used.  So do not test with it.

set torture_eval_before_compile {
  if {[string match {*-fomit-frame-pointer*} "$option"]} {
    continue
  }
}

return 0
