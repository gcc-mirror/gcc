! { dg-do run )
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR90218. The errors that occurred are indicated by the comments below.
! They have all been fixed and the testcase no longer leaks memory.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
  type :: foo(a)
    integer,len :: a
    integer :: vals(a)
  end type
  type :: bar(b)
    integer,len :: b
    type(foo(2)) :: array(b)
  end type

  type :: barbar(b)
    integer,len :: b
    type(foo(2)), allocatable :: array(:)
  end type


  type(barbar(2)) :: var2
  type(bar(2)) :: var = bar(2)([foo(2)([1,2]),foo(2)([3,4])])    ! Values were not set

  if (any (var%array%vals(1) /= [1,3])) stop 1
  if (any (var%array%vals(2) /= [2,4])) stop 2

  var = bar(2)([foo(2)(-[1,2]),foo(2)(-[3,4])])                  ! Was OK but 16bytes/2 blocks lost

  var%array = [foo(2)([5,6]),foo(2)([7,8])]                      ! Was an invalid free here
                                                                 ! also 16bytes/2 blocks lost
  if (any (var%array%vals(1) /= [5,7])) stop 3
  if (any (var%array%vals(2) /= [6,8])) stop 4

  var2 = barbar(2)([foo(2)([1,2]),foo(2)([3,4])])                ! 16bytes/2 blocks lost

  if (any (var2%array%vals(1) /= [1,3])) stop 5
  if (any (var2%array%vals(2) /= [2,4])) stop 6
  if (allocated (var2%array)) deallocate (var2%array)            ! Caused gimplifier problems

  call foobar

contains
  subroutine foobar
  type(barbar(2)) :: var_s2
  type(bar(2)) :: var_s = bar(2)([foo(2)([1,2]),foo(2)([3,4])])  ! Values were not set

  if (any (var_s%array%vals(1) /= [1,3])) stop 1
  if (any (var_s%array%vals(2) /= [2,4])) stop 2

  var_s = bar(2)([foo(2)(-[1,2]),foo(2)(-[3,4])])                ! Was OK but 16bytes/2 blocks lost

  var_s%array = [foo(2)([5,6]),foo(2)([7,8])]                    ! Was an invalid free here
                                                                 ! also 16bytes/2 blocks lost
  if (any (var_s%array%vals(1) /= [5,7])) stop 3
  if (any (var_s%array%vals(2) /= [6,8])) stop 4

  var_s2 = barbar(2)([foo(2)([1,2]),foo(2)([3,4])])              ! 16bytes/2 blocks lost

  if (any (var_s2%array%vals(1) /= [1,3])) stop 5
  if (any (var_s2%array%vals(2) /= [2,4])) stop 6
  if (allocated (var_s2%array)) deallocate (var_s2%array)        ! Caused gimplifier problems
  end

end                                                              ! 160bytes/1 block was lost here
! { dg-final { scan-tree-dump-times "__builtin_malloc" 30 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 33 "original" } }
