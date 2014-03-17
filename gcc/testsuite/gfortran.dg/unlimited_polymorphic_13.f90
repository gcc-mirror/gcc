! { dg-do run }
!
! PR fortran/58793
!
! Contributed by  Vladimir Fuka
!
! Had the wrong value for the storage_size for complex
!
module m
  use iso_fortran_env
  implicit none
  integer, parameter :: c1 = real_kinds(1)
  integer, parameter :: c2 = real_kinds(2)
  integer, parameter :: c3 = real_kinds(size(real_kinds)-1)
  integer, parameter :: c4 = real_kinds(size(real_kinds))
  real(c1) :: r1
  real(c2) :: r2
  real(c3) :: r3
  real(c4) :: r4
contains
 subroutine s(o, k)
    class(*) :: o
    integer :: k
    integer :: sz

    sz = 0
    select case (k)
     case (4)
      sz = storage_size(r1)*2
    end select
    select case (k)
     case (8)
      sz = storage_size(r2)*2
    end select
    select case (k)
     case (real_kinds(size(real_kinds)-1))
      sz = storage_size(r3)*2
    end select
    select case (k)
     case (real_kinds(size(real_kinds)))
      sz = storage_size(r4)*2
    end select
    if (sz .eq. 0) call abort()

    if (storage_size(o) /= sz) call abort()

! Break up the SELECT TYPE to pre-empt collisions in the value of 'cn'
    select type (o)
      type is (complex(c1))
        if (storage_size(o) /= sz) call abort()
    end select
    select type (o)
      type is (complex(c2))
        if (storage_size(o) /= sz) call abort()
    end select
    select type (o)
      type is (complex(c3))
        if (storage_size(o) /= sz) call abort()
     end select
    select type (o)
     type is (complex(c4))
        if (storage_size(o) /= sz) call abort()
    end select
  end subroutine s
end module m

program p
 use m
 call s((1._c1, 2._c1), c1)
 call s((1._c2, 2._c2), c2)
 call s((1._c3, 2._c3), c3)
 call s((1._c4, 2._c4), c4)
end program p
