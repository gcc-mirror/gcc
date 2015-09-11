! { dg-do compile }
!
! PR fortran/64943
!
! Contributed Dominique d'Humieres
!
  type :: Test
    integer :: i
  end type

  type :: TestReference
     class(Test), allocatable :: test(:)
  end type
print *, TestReference([Test(99), Test(199)]) ! { dg-error "Data transfer element at .1. cannot have ALLOCATABLE components unless it is processed by a defined input/output procedure" }
end
