# Only Linux does inlclude all c99 functions at the moment.
if { ! [istarget "*linux*"] } { return 1 }
return 0
