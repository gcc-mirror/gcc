! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_3.c }
!
! Test the fix for PR88929.
!
  integer, dimension (:,:), allocatable :: actual
  integer, dimension(2,2) :: src = reshape ([1,2,3,4], [2,2])

  allocate (actual, source = src)
  ier = test1 (actual)
  if (ier .ne. 0) stop 1
! C call is INTENT(IN). 'c_test' increments elements of 'src'.
  if (any (actual .ne. src)) stop 2

  ier = test2 (actual)
  if (ier .ne. 0) stop 1
! C call is INTENT(INOUT) 'c_test' increments elements of 'src'.
  if (any (actual .ne. src + 1)) stop 2

contains

  function test1 (arg) RESULT(err)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(C_INT) :: err
    type(*), dimension(..), intent(inOUT) :: arg
    interface
      function test_c (a) BIND(C, NAME="c_test") RESULT(err)
          USE, INTRINSIC :: ISO_C_BINDING
          type(*), dimension(..), intent(in) :: a
          INTEGER(C_INT) :: err
      end function
    end interface

    err = test_c (arg) ! This used to ICE

  end function test1

  function test2 (arg) RESULT(err)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(C_INT) :: err
    type(*), dimension(..), intent(inout) :: arg
    interface
      function test_c (a) BIND(C, NAME="c_test") RESULT(err)
          USE, INTRINSIC :: ISO_C_BINDING
          type(*), dimension(..), intent(inout) :: a
          INTEGER(C_INT) :: err
      end function
    end interface

    err = test_c (arg) ! This used to ICE

  end function test2
end
