# Use "-mtune=i686" on i?86-*-* unless "-m64" is specified.
if { [istarget "i?86-*-*"] } {
  set target_name [target_info name]
  if {[board_info $target_name exists multilib_flags]} {
    set multilib_flags [board_info $target_name multilib_flags]
    if { ![regexp -- "-m64" $multilib_flags] } {
      set additional_flags "-mtune=i686"
    }
  } else {
    set additional_flags "-mtune=i686"
  }
}
return 0
