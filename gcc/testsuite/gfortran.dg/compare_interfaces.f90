! { dg-do compile }
!
! Contributed by Mark Eggleston  <mark.eggleston@codethink.co.uk>

subroutine f(a, b)
  integer :: a
  real :: b

  write(*,*) a, b
end subroutine

subroutine g(a, b)
  integer :: a
  character(*) :: b

  write(*,*) a, b
end subroutine

subroutine h
  interface
    subroutine f(a, b)  ! { dg-error "\\(CHARACTER\\(\\*\\)/REAL\\(4\\)\\)" }
      integer :: a
      character(*) :: b
    end subroutine
    subroutine g(a, b)  ! { dg-error "\\(REAL\\(4\\)/CHARACTER\\(\\*\\)\\)" }
      integer :: a
      real :: b
    end subroutine
  end interface

  call f(6, 6.0)
  call g(6, "abcdef")
end subroutine

subroutine f4(a, b)
  integer :: a
  real :: b

  write(*,*) a, b
end subroutine

subroutine g4(a, b)
  integer :: a
  character(*,4) :: b

  write(*,*) a, b
end subroutine

subroutine h4
  interface
    subroutine f4(a, b)  ! { dg-error "\\(CHARACTER\\(\\*,4\\)/REAL\\(4\\)\\)" }
      integer :: a
      character(*,4) :: b
    end subroutine
    subroutine g4(a, b)  ! { dg-error "REAL\\(4\\)/CHARACTER\\(\\*,4\\)" }
      integer :: a
      real :: b
    end subroutine
  end interface

  call f4(6, 6.0) 
  call g4(6, 4_"abcdef")
end subroutine

program test
  call h
  call h4
end program

! { dg-error "passed REAL\\(4\\) to CHARACTER\\(\\*\\)" "type mismatch" { target \*-\*-\* } 31 }
! { dg-error "passed CHARACTER\\(6\\) to REAL\\(4\\)" "type mismatch" { target \*-\*-\* } 32 }
! { dg-error "passed REAL\\(4\\) to CHARACTER\\(\\*,4\\)" "type mismatch" { target \*-\*-\* } 61 }
! { dg-error "passed CHARACTER\\(6,4\\) to REAL\\(4\\)" "type mismatch" { target \*-\*-\* } 62 }
