! this is the reduced testcase from pr13372
! we wrongly add a symbol "P" to the module
! Currently (2004/06/09) a workaround is in place
! PR 15481 tracks any steps towards a real fix.
module typeSizes
implicit none
  integer, parameter :: FourByteReal = selected_real_kind(P =  6, R =  37)
end module typeSizes
