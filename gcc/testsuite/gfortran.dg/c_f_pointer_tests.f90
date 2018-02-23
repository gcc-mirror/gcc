! { dg-do run }
! { dg-additional-sources c_f_tests_driver.c }
module c_f_pointer_tests
  use, intrinsic :: iso_c_binding

  type myF90Derived
     integer(c_int) :: cInt
     real(c_double) :: cDouble
     real(c_float) :: cFloat
     integer(c_short) :: cShort
     type(c_funptr) :: myFunPtr
  end type myF90Derived

  type dummyDerived
     integer(c_int) :: myInt
  end type dummyDerived

  contains

  subroutine testDerivedPtrs(myCDerived, derivedArray, arrayLen, &
       derived2DArray, dim1, dim2) &
       bind(c, name="testDerivedPtrs")
    implicit none
    type(c_ptr), value :: myCDerived
    type(c_ptr), value :: derivedArray
    integer(c_int), value :: arrayLen
    type(c_ptr), value :: derived2DArray
    integer(c_int), value :: dim1
    integer(c_int), value :: dim2
    type(myF90Derived), pointer :: myF90Type
    type(myF90Derived), dimension(:), pointer :: myF90DerivedArray
    type(myF90Derived), dimension(:,:), pointer :: derivedArray2D
    ! one dimensional array coming in (derivedArray)
    integer(c_int), dimension(1:1) :: shapeArray
    integer(c_int), dimension(1:2) :: shapeArray2
    type(myF90Derived), dimension(1:10), target :: tmpArray

    call c_f_pointer(myCDerived, myF90Type)
    ! make sure numbers are ok.  initialized in c_f_tests_driver.c
    if(myF90Type%cInt .ne. 1) then
       STOP 1
    endif
    if(myF90Type%cDouble .ne. 2.0d0) then
       STOP 2
    endif
    if(myF90Type%cFloat .ne. 3.0) then
       STOP 3
    endif
    if(myF90Type%cShort .ne. 4) then
       STOP 4
    endif

    shapeArray(1) = arrayLen
    call c_f_pointer(derivedArray, myF90DerivedArray, shapeArray)

    ! upper bound of each dim is arrayLen2
    shapeArray2(1) = dim1
    shapeArray2(2) = dim2
    call c_f_pointer(derived2DArray, derivedArray2D, shapeArray2)
    ! make sure the last element is ok
    if((derivedArray2D(dim1, dim2)%cInt .ne. 4) .or. &
         (derivedArray2D(dim1, dim2)%cDouble .ne. 4.0d0) .or. &
         (derivedArray2D(dim1, dim2)%cFloat .ne. 4.0) .or. &
         (derivedArray2D(dim1, dim2)%cShort .ne. 4)) then
       STOP 5
    endif
  end subroutine testDerivedPtrs
end module c_f_pointer_tests
