! { dg-do compile }
! { dg-options "-fmax-errors=1000 -fopenmp -ffree-line-length-160" }

module udr6
  type dt
    integer :: i
  end type
end module udr6
subroutine f1
  use udr6, only : dt
!$omp declare reduction (+:integer:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (+:real(kind=4):omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (+:double precision:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (+:integer(kind=8),integer(kind=1) & ! { dg-error "Redefinition of predefined" }
!$omp & :omp_out = omp_out + omp_in)
!$omp declare reduction (+:complex:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (+:complex(kind=8):omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(+)
    function addf1 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: addf1
    end function
  end interface
end subroutine f1
subroutine f2
  use udr6, only : dt
  interface operator(-)
    function subf2 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: subf2
    end function
  end interface
!$omp declare reduction (-:integer:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (-:real(kind=4):omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (-:double precision:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (-:integer(kind=8),integer(kind=1) & ! { dg-error "Redefinition of predefined" }
!$omp & :omp_out = omp_out + omp_in)
!$omp declare reduction (-:complex:omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (-:complex(kind=8):omp_out = omp_out + omp_in) ! { dg-error "Redefinition of predefined" }
end subroutine f2
subroutine f3
  use udr6, only : dt
  interface operator(*)
    function mulf3 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: mulf3
    end function
  end interface
!$omp declare reduction (*:integer:omp_out = omp_out * omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (*:real(kind=4):omp_out = omp_out * omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (*:double precision:omp_out = omp_out * omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (*:integer(kind=8),integer(kind=1) & ! { dg-error "Redefinition of predefined" }
!$omp & :omp_out = omp_out * omp_in)
!$omp declare reduction (*:complex:omp_out = omp_out * omp_in) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (*:complex(kind=8):omp_out = omp_out * omp_in) ! { dg-error "Redefinition of predefined" }
end subroutine f3
subroutine f4
  use udr6, only : dt
  interface operator(.and.)
    function andf4 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: andf4
    end function
  end interface
!$omp declare reduction (.neqv.:logical:omp_out = omp_out .or. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.or.)
    function orf4 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: orf4
    end function
  end interface
!$omp declare reduction (.eqv.:logical:omp_out = omp_out .or. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.eqv.)
    function eqvf4 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: eqvf4
    end function
  end interface
!$omp declare reduction (.or.:logical:omp_out = omp_out .or. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.neqv.)
    function neqvf4 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: neqvf4
    end function
  end interface
!$omp declare reduction (.and.:logical:omp_out = omp_out .and. omp_in) ! { dg-error "Redefinition of predefined" }
end subroutine f4
subroutine f5
  use udr6, only : dt
  interface operator(.and.)
    function andf5 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: andf5
    end function
  end interface
!$omp declare reduction (.neqv.:logical(kind =4):omp_out = omp_out .neqv. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.or.)
    function orf5 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: orf5
    end function
  end interface
!$omp declare reduction (.eqv.:logical(kind= 4):omp_out = omp_out .eqv. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.eqv.)
    function eqvf5 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: eqvf5
    end function
  end interface
!$omp declare reduction (.or.:logical(kind=4):omp_out = omp_out .or. omp_in) ! { dg-error "Redefinition of predefined" }
  interface operator(.neqv.)
    function neqvf5 (x, y)
      use udr6, only : dt
      type(dt), intent (in) :: x, y
      type(dt) :: neqvf5
    end function
  end interface
!$omp declare reduction (.and.:logical(kind = 4):omp_out = omp_out .and. omp_in) ! { dg-error "Redefinition of predefined" }
end subroutine f5
subroutine f6
!$omp declare reduction (min:integer:omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:integer:omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (iand:integer:omp_out = iand (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (ior:integer:omp_out = ior (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (ieor:integer:omp_out = ieor (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (min:real:omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:real:omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (min:double precision:omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:double precision:omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
end subroutine f6
subroutine f7
!$omp declare reduction (min:integer(kind=2):omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:integer(kind=4):omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (iand:integer(kind=1):omp_out = iand (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (ior:integer(kind=8):omp_out = ior (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (ieor:integer(kind=4):omp_out = ieor (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (min:real(kind=4):omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:real(kind=4):omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (min:double precision:omp_out = min (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
!$omp declare reduction (max:double precision:omp_out = max (omp_out, omp_in)) ! { dg-error "Redefinition of predefined" }
end subroutine f7
subroutine f8
  integer :: min
!$omp declare reduction (min:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (min:real:omp_out = omp_out + omp_in)
!$omp declare reduction (min:double precision:omp_out = omp_out + omp_in)
end subroutine f8
subroutine f9
  integer :: max
!$omp declare reduction (max:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (max:real:omp_out = omp_out + omp_in)
!$omp declare reduction (max:double precision:omp_out = omp_out + omp_in)
end subroutine f9
subroutine f10
  integer :: iand
!$omp declare reduction (iand:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (iand:real:omp_out = omp_out + omp_in)
end subroutine f10
subroutine f11
  integer :: ior
!$omp declare reduction (ior:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (ior:real:omp_out = omp_out + omp_in)
end subroutine f11
subroutine f12
  integer :: ieor
!$omp declare reduction (ieor:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (ieor:real:omp_out = omp_out + omp_in)
end subroutine f12
subroutine f13
!$omp declare reduction (min:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (min:real:omp_out = omp_out + omp_in)
!$omp declare reduction (min:double precision:omp_out = omp_out + omp_in)
  integer :: min
end subroutine f13
subroutine f14
!$omp declare reduction (max:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (max:real:omp_out = omp_out + omp_in)
!$omp declare reduction (max:double precision:omp_out = omp_out + omp_in)
  integer :: max
end subroutine f14
subroutine f15
!$omp declare reduction (iand:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (iand:real:omp_out = omp_out + omp_in)
  integer :: iand
end subroutine f15
subroutine f16
!$omp declare reduction (ior:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (ior:real:omp_out = omp_out + omp_in)
  integer :: ior
end subroutine f16
subroutine f17
!$omp declare reduction (ieor:integer:omp_out = omp_out + omp_in)
!$omp declare reduction (ieor:real:omp_out = omp_out + omp_in)
  integer :: ieor
end subroutine f17
