# XFAIL the run for m64 Darwin NeXT (seems to be a system runtime lib problem).
if { [istarget *-*-darwin*] && [check_effective_target_lp64] } {
    set torture_eval_before_execute {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
          "Target fails for fnext-runtime" "*-*-*" { "-fnext-runtime" } { "" }
        }
    }
}
# carry on...
return false
