subroutine foo
  type t
    integer :: i, j
  end type t

  type t2
    type(t) :: cc(3)
  end type t2

  type(t) x, y(3)
  type(t2) :: z(3)

  ! OK - map whole aggregated variable
!$acc enter data copyin(x)
  ! map(to:x [len: 8])

  ! OK - map two components of the aggregated variable
!$acc enter data copyin(x%j, x%i)

  ! Bad - we cannot mix full-object and component accesses
!$acc enter data copyin(x, x%i)
! { dg-error "Symbol .x. has mixed component and non-component accesses" "" { target "*-*-*" } 21 }

  ! Bad - we cannot do a strided access of 'x'
  ! No C/C++ equivalent
!$acc enter data copyin(y(:)%i)
! { dg-error "not a proper array section" "" { target "*-*-*" } 26 }

  ! Bad - again, a strided access
!$acc enter data copyin(z(1)%cc(:)%i)
! { dg-error "not a proper array section" "" { target "*-*-*" } 30 }
end
