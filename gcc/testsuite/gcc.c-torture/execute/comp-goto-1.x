if {[istarget "*arm-*-*"] || [istarget "thumb-*-*"]} {

   # On the Arm specifying -g produces a bogus label reference
   # in debugging output.

    set torture_eval_before_compile {
      set additional_flags "-g0"
    }
}

return 0
