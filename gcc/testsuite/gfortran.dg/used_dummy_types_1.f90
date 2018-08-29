! { dg-do run }
! This checks the fix for PR20244 in which USE association
! of derived types would cause an ICE, if the derived type
! was also available by host association. This occurred 
! because the backend declarations were different. 
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!==============
module mtyp
  type t1
     integer::a
  end type t1
end module mtyp
!==============
module atest
  use mtyp
  type(t1)::ze
contains
  subroutine test(ze_in )
    use mtyp
    implicit none
    type(t1)::ze_in
    ze_in = ze
  end subroutine test
  subroutine init( )
    implicit none
    ze = t1 (42)
  end subroutine init
end module atest
!==============
  use atest
  type(t1) :: res = t1 (0)
  call init ()
  call test (res)
  if (res%a.ne.42) STOP 1
end  
