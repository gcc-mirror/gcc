! { dg-do compile }
!
! PR 86935: Bad locus in ASSOCIATE statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

type :: t
  real :: r = 0.5
  integer :: i = 3
end type

type(t) :: x

associate (r => x%r, &
           i => x%ii)   ! { dg-error "Invalid association target" }

end
