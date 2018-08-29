! { dg-do run }
!
! Check that allocate with source for arrays without array-spec
! works.
! PR fortran/44672
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!                Antony Lewis  <antony@cosmologist.info>
!                Andre Vehreschild  <vehre@gcc.gnu.org>
!

program allocate_with_source_6

  type P
    class(*), allocatable :: X(:,:)
  end type

  type t
  end type t

  type(t), allocatable :: a(:), b, c(:)
  integer :: num_params_used = 6
  integer, allocatable :: m(:)

  allocate(b,c(5))
  allocate(a(5), source=b)
  deallocate(a)
  allocate(a, source=c)
  allocate(m, source=[(I, I=1, num_params_used)])
  if (any(m /= [(I, I=1, num_params_used)])) STOP 1
  deallocate(a,b,m)
  call testArrays()

contains
  subroutine testArrays()
    type L
      class(*), allocatable :: v(:)
    end type
    Type(P) Y
    type(L) o
    real arr(3,5)
    real, allocatable :: v(:)

    arr = 5
    allocate(Y%X, source=arr)
    select type (R => Y%X)
      type is (real)
        if (any(reshape(R, [15]) /= [5,5,5,5,5, 5,5,5,5,5, 5,5,5,5,5])) &
          STOP 2
      class default
        STOP 3
    end select
    deallocate(Y%X)

    allocate(Y%X, source=arr(2:3,3:4))
    select type (R => Y%X)
      type is (real)
        if (any(reshape(R, [4]) /= [5,5,5,5])) &
          STOP 4
      class default
        STOP 5
    end select
    deallocate(Y%X)

    allocate(o%v, source=arr(2,3:4))
    select type (R => o%v)
      type is (real)
        if (any(R /= [5,5])) &
          STOP 6
      class default
        STOP 7
    end select
    deallocate(o%v)

    allocate(v, source=arr(2,1:5))
    if (any(v /= [5,5,5,5,5])) STOP 8
    deallocate(v)
  end subroutine testArrays
end

