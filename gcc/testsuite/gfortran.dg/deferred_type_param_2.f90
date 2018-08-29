! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/45170
!
! Character deferred type parameter
!

subroutine one(x, y) ! { dg-error "Entity .y. at .1. has a deferred type parameter" }
  implicit none
  character(len=:), pointer :: x
  character(len=:) :: y
  character(len=:), allocatable, target :: str2
  character(len=:), target :: str ! { dg-error "deferred type parameter" }
end subroutine one

subroutine two()
  implicit none
  character(len=:), allocatable, target :: str1(:)
  character(len=5), save, target :: str2
  character(len=:), pointer :: pstr => str2
  character(len=:), pointer :: pstr2(:)
end subroutine two

subroutine three()
!  implicit none  ! Disabled because of PR 46152
  character(len=:), allocatable, target :: str1(:)
  character(len=5), save, target :: str2
  character(len=:), pointer :: pstr
  character(len=:), pointer :: pstr2(:)

  pstr => str2
  pstr2 => str1
  str1 = ["abc"]
  pstr2 => str1

  allocate (character(len=77) :: str1(1))
  allocate (pstr, source=str2)
  allocate (pstr, mold=str2)
  allocate (pstr) ! { dg-error "requires either a type-spec or SOURCE tag" }
  allocate (character(len=:) :: str1(1)) ! { dg-error "cannot contain a deferred type parameter" }

  str1 = [ character(len=2) :: "abc" ]
  str1 = [ character(len=:) :: "abc" ] ! { dg-error "cannot contain a deferred type parameter" }
end subroutine three

subroutine four()
  implicit none
  character(len=:), allocatable, target :: str
  character(len=:), pointer :: pstr
  pstr => str
  str = "abc"
  if(len(pstr) /= len(str) .or. len(str)/= 3) STOP 1
  str = "abcd"
  if(len(pstr) /= len(str) .or. len(str)/= 4) STOP 2
end subroutine four

subroutine five()
character(len=4) :: str*(:)
allocatable :: str
end subroutine five

