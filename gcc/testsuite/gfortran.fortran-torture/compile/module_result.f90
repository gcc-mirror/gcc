! Result variables in module procedures
module module_result
   implicit none
contains
function test () result (res)
   integer res
   res = 0
end function
end module
