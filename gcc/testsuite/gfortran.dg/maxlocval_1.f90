! { dg-do run }
! Check that maxval uses for integers HUGE()-1.
! PR fortran/30512

program main
implicit none
integer(1) :: i1(3), a1(3:2)
integer(2) :: i2(3), a2(3:2)
integer(4) :: i4(3), a4(3:2)
integer(8) :: i8(3), a8(3:2)

integer(kind=4), allocatable :: a(:,:)
integer(kind=8), allocatable :: b(:,:)

logical :: msk(3)
msk = .false.

i1 = 1
i2 = 1
i4 = 1
i8 = 1

if(-huge(i1)-1_1 /= maxval(i1, msk)) STOP 1 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(-huge(a1)-1_1 /= maxval(a1))      STOP 2 ! { dg-warning "outside symmetric range implied by Standard Fortran" }

if(-huge(i2)-1_2 /= maxval(i2, msk)) STOP 3 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(-huge(a2)-1_2 /= maxval(a2))      STOP 4 ! { dg-warning "outside symmetric range implied by Standard Fortran" }

if(-huge(i4)-1_4 /= maxval(i4, msk)) STOP 5 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(-huge(a4)-1_4 /= maxval(a4))      STOP 6 ! { dg-warning "outside symmetric range implied by Standard Fortran" }

if(-huge(i8)-1_4 /= maxval(i8, msk)) STOP 7 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(-huge(a8)-1_4 /= maxval(a8))      STOP 8 ! { dg-warning "outside symmetric range implied by Standard Fortran" }

allocate (a(0:-1,1:1))
allocate (b(0:-1,1:1))

if(any(maxval(a,dim=1) /= -huge(a)-1_4)) STOP 9 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(any(minval(a,dim=1) /=  huge(a)    )) STOP 10

if(any(maxval(b,dim=1) /= -huge(b)-1_8)) STOP 11 ! { dg-warning "outside symmetric range implied by Standard Fortran" }
if(any(minval(b,dim=1) /=  huge(b)    )) STOP 12

end program main
