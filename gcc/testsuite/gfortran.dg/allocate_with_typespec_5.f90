! { dg-do compile }
!
! PR fortran/51652
!
! Contributed by David Kinniburgh
!
module settings

type keyword
  character(60), allocatable :: c(:)
end type keyword

type(keyword) :: kw(10)

contains

subroutine save_kw
  allocate(character(80) :: kw(1)%c(10)) ! { dg-error "with type-spec requires the same character-length parameter" }
end subroutine save_kw

subroutine foo(n)
  character(len=n+2), allocatable :: x
  allocate (character(len=n+3) :: x) ! { dg-error "type-spec requires the same character-length parameter" }
end subroutine foo

end module settings
