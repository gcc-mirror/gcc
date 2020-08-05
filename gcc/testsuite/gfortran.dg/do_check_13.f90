program main
  implicit none
  integer :: i1, i2, i3, i4, i5, i6, i7
  integer :: j
  do i1=1,10
     call sub1 ! { dg-error "Index variable 'i1' redefined" }
  end do
  do i2=1,10
     call sub2 ! { dg-error "Index variable 'i2' redefined" }
  end do
  do i3=1,10
     j = fcn3() ! { dg-error "Index variable 'i3' redefined" }
  end do
  do i4=1,10
     j = fcn4() ! { dg-error "Index variable 'i4' redefined" }
  end do
  do i5=1,10
     call sub5 ! { dg-error "Index variable 'i5' set to undefined" }
  end do

  call sub6

  do i7=1,10
     call sub7 ! { dg-error "Index variable 'i7' not definable" }
  end do
contains
  subroutine sub1
    i1 = 5 ! { dg-error "Index variable 'i1' redefined" }
  end subroutine sub1

  subroutine sub2
    do i2=1,5 ! { dg-error "Index variable 'i2' redefined" }
    end do
  end subroutine sub2
  
  integer function fcn3()
    i3 = 1 ! { dg-error "Index variable 'i3' redefined" }
    fcn3 = i3
  end function fcn3

  integer function fcn4()
    open (10,file="foo.dat", iostat=i4) ! { dg-error "Index variable 'i4' redefined" }
    fcn4 = 12
  end function fcn4

  subroutine sub5
    integer :: k
    k = intentout(i5) ! { dg-error "Index variable 'i5' set to undefined" }
  end subroutine sub5

  subroutine sub6
    do i6=1,10
       call sub6a ! { dg-error "Index variable 'i6' redefined" }
    end do
  end subroutine sub6

  subroutine sub6a
    i6 = 5   ! { dg-error "Index variable 'i6' redefined" }
  end subroutine sub6a

  subroutine sub7
    integer :: k
    k = intentinout (i7)  ! { dg-error "Index variable 'i7' not definable" }
  end subroutine sub7
  
  integer function intentout(i)
    integer, intent(out) :: i
  end function intentout

  integer function intentinout(i)
    integer, intent(inout) :: i
  end function intentinout
end program main

module foo
  integer :: j1
contains
  subroutine mod_sub_1
    do j1=1,10
       call aux ! { dg-error "Index variable 'j1' redefined" }
    end do
  end subroutine mod_sub_1
  subroutine aux
    j1 = 3  ! { dg-error "Index variable 'j1' redefined" }
  end subroutine aux
end module foo
