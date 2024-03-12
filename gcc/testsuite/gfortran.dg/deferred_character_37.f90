! { dg-do run }
! PR fortran/95947
! PR fortran/110658
!
! Test deferred-length character arguments to selected intrinsics
! that may return a character result of same length as first argument:
! CSHIFT, EOSHIFT, MAXVAL, MERGE, MINVAL, PACK, SPREAD, TRANSPOSE, UNPACK

program p
  implicit none
  call pr95947 ()
  call pr110658 ()
  call s ()

contains

  subroutine pr95947
    character(len=:), allocatable :: m(:)

    m = [ character(len=10) :: 'ape','bat','cat','dog','eel','fly','gnu']
    m = pack (m, mask=(m(:)(2:2) == 'a'))

!   print *, "m = '", m,"' ",               "; expected is ['bat','cat']"
    if (.not. all (m == ['bat','cat'])) stop 1
   
!   print *, "size(m) =     ", size(m),     "; expected is 2"
    if (size (m) /= 2) stop 2
   
!   print *, "len(m) =      ", len(m),      "; expected is 10"
    if (len (m) /= 10) stop 3
   
!   print *, "len_trim(m) = ", len_trim(m), "; expected is 3 3"
    if (.not. all (len_trim(m) == [3,3])) stop 4
  end

  subroutine pr110658
    character(len=:), allocatable :: array(:), array2(:,:)
    character(len=:), allocatable :: res, res1(:), res2(:)

    array = ["bb", "aa", "cc"]

    res = minval (array)
    if (res /= "aa") stop 11

    res = maxval (array, mask=[.true.,.true.,.false.])
    if (res /= "bb") stop 12

    res1 = cshift (array, 1)
    if (any (res1 /= ["aa","cc","bb"])) stop 13

    res2 = eoshift (res1, -1)
    if (any (res2 /= ["  ", "aa", "cc"])) stop 14

    res2 = pack (array, mask=[.true.,.false.,.true.])
    if (any (res2 /= ["bb","cc"])) stop 15

    res2 = unpack (res2, mask=[.true.,.false.,.true.], field="aa")
    if (any (res2 /= array)) stop 16

    res2 = merge (res2, array, [.true.,.false.,.true.])
    if (any (res2 /= array)) stop 17

    array2 = spread (array, dim=2, ncopies=2)
    array2 = transpose (array2)
    if (any (shape (array2) /= [2,3])) stop 18
    if (any (array2(2,:) /= array))    stop 19
  end

  subroutine s
    character(:), allocatable :: array1(:), array2(:)
    array1 = ["aa","cc","bb"]
    array2 = copy (array1)
    if (any (array1 /= array2)) stop 20
  end

  function copy (arg) result (res)
    character(:), allocatable :: res(:)
    character(*), intent(in)  :: arg(:)
    integer :: i, k, n
    k = len (arg)
    n = size (arg)
    allocate (character(k) :: res(n))
    do i = 1, n
       res(i) = arg(i)
    end do
  end

end
