# Only glibc includes all c99 functions at the moment.
if { ! [istarget "*-linux*"] } { return 1 }
if { [check_effective_target_uclibc] } { return 1 }
return 0
