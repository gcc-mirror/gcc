! { dg-do run }
!
! Contributed by Reinhold Bader
!
program assumed_shape_01
  implicit none
  type :: cstruct
     integer :: i
     real :: r(2)
  end type cstruct

  type(cstruct), pointer :: u(:)
  integer, allocatable :: iv(:), iv2(:)
  integer, allocatable :: im(:,:)
  integer, parameter :: cim(2,3) = reshape([1,2,3, 2,3,4], [2,3])
  integer :: i
  integer, parameter :: lcim(2,10) = reshape([(i, i=1,10),(i,i=1,10)], [2,10])

  allocate(iv, source= [ 1, 2, 3, 4])
  if (any(iv /= [ 1, 2, 3, 4])) STOP 1
  deallocate(iv)

  allocate(iv, source=(/(i, i=1,10)/))
  if (any(iv /= (/(i, i=1,10)/))) STOP 2

  ! Now 2D
  allocate(im, source= cim)
  if (any(im /= cim)) STOP 3
  deallocate(im)

  allocate(im, source= reshape([iv, iv], [2, size(iv, 1)]))
  if (any(im /= lcim)) STOP 4
  deallocate(im)
  deallocate(iv)

  allocate(u, source=[cstruct( 4, [1.1,2.2] )] )
  if (any(u(:)%i /= 4) .or. any(abs(u(1)%r(:) - [1.1,2.2]) > 1E-6)) STOP 5
  deallocate (u)

  allocate(iv, source= arrval())
  if (any(iv /= [ 1, 2, 4, 5, 6])) STOP 6
  ! Check simple array assign
  allocate(iv2, source=iv)
  if (any(iv2 /= [ 1, 2, 4, 5, 6])) STOP 7
  deallocate(iv, iv2)

  ! Now check for mold=
  allocate(iv, mold= [ 1, 2, 3, 4])
  if (any(shape(iv) /= [4])) STOP 8
  deallocate(iv)

  allocate(iv, mold=(/(i, i=1,10)/))
  if (any(shape(iv) /= [10])) STOP 9

  ! Now 2D
  allocate(im, mold= cim)
  if (any(shape(im) /= shape(cim))) STOP 10
  deallocate(im)

  allocate(im, mold= reshape([iv, iv], [2, size(iv, 1)]))
  if (any(shape(im) /= shape(lcim))) STOP 11
  deallocate(im)
  deallocate(iv)

  allocate(u, mold=[cstruct( 4, [1.1,2.2] )] )
  if (any(shape(u(1)%r(:)) /= 2)) STOP 12
  deallocate (u)

  allocate(iv, mold= arrval())
  if (any(shape(iv) /= [5])) STOP 13
  ! Check simple array assign
  allocate(iv2, mold=iv)
  if (any(shape(iv2) /= [5])) STOP 14
  deallocate(iv, iv2)

  call addData([4, 5])
  call addData(["foo", "bar"])
contains
  function arrval()
    integer, dimension(5) :: arrval
    arrval = [ 1, 2, 4, 5, 6]
  end function

  subroutine addData(P)
    class(*), intent(in) :: P(:)
    class(*), allocatable :: cP(:)
    allocate (cP, source= P)
    select type (cP)
      type is (integer)
        if (any(cP /= [4,5])) STOP 15
      type is (character(*))
        if (len(cP) /= 3) STOP 16
        if (any(cP /= ["foo", "bar"])) STOP 17
      class default
        STOP 18
    end select
    deallocate (cP)
    allocate (cP, mold= P)
    select type (cP)
      type is (integer)
        if (any(size(cP) /= [2])) STOP 19
      type is (character(*))
        if (len(cP) /= 3) STOP 20
        if (any(size(cP) /= [2])) STOP 21
      class default
        STOP 22
    end select
    deallocate (cP)
  end subroutine
end program assumed_shape_01
