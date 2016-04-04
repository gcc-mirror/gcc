! { dg-do run }

program truc
implicit none

type t_env_table
    character(len=:), allocatable :: key
end type

type(t_env_table), dimension(:), allocatable :: environment_table

character(len=:), allocatable :: s

allocate(environment_table(1))
environment_table(1)%key='tt'

allocate(s, source=environment_table(1)%key)

if ( .not. allocated(s) ) call abort()
if ( s /= "tt" ) call abort()
if ( len(s) /= 2 ) call abort()
!print *, 's:"', s, '" derived:"',environment_table(1)%key,'"'

end program
