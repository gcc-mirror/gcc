! { dg-do compile }
! PR 85641 - this used to ICE due do infinite recursion.
! Test case by Antony Lewis.
program tester
character(LEN=:), allocatable :: fields
integer j
character(LEN=4), parameter :: CMB_CL_Fields = 'TEBP'

fields = ''
j=1
fields = fields // CMB_CL_Fields(j:j)

end program tester
