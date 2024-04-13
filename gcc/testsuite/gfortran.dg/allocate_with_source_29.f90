! { dg-do compile }
! { dg-options "-std=f2008" }
! PR fortran/113793
!
! Static checks of string length for ALLOCATE with SOURCE= or MOLD=

program p
  implicit none
  character(kind=1,len=8), allocatable :: a(:), d, b(:,:)
  character(kind=4,len=6), allocatable :: c(:), e, f(:,:)
  character(kind=1,len=2)              :: c1 = "xx"
  character(kind=1,len=8)              :: c2 = "yy"
  character(kind=4,len=6)              :: c3 = 4_"ww"
  character(kind=4,len=3)              :: c4 = 4_"zz"

  ALLOCATE (a(1),source=  "a")       ! { dg-error "Unequal character lengths .8/1. " }
  ALLOCATE (a(2),mold  =  "bb")      ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (c(3),source=4_"yyy")     ! { dg-error "Unequal character lengths .6/3. " }
  ALLOCATE (c(4),mold  =4_"zzzz")    ! { dg-error "Unequal character lengths .6/4. " }
  ALLOCATE (d,   source=  "12345")   ! { dg-error "Unequal character lengths .8/5. " }
  ALLOCATE (d,   source=  "12345678")
  ALLOCATE (d,   mold  =  "123456")  ! { dg-error "Unequal character lengths .8/6. " }
  ALLOCATE (e,   source=4_"654321")
  ALLOCATE (e,   mold  =4_"7654321") ! { dg-error "Unequal character lengths .6/7. " }
  ALLOCATE (a(5),source=c1)          ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (a(6),mold  =c1)          ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (c(7),source=c4)          ! { dg-error "Unequal character lengths .6/3. " }
  ALLOCATE (c(8),mold  =c4)          ! { dg-error "Unequal character lengths .6/3. " }
  ALLOCATE (a,source=[c1,c1,c1])     ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (a,source=[c2,c2,c2])
  ALLOCATE (c,source=[c3,c3])
  ALLOCATE (c,source=[c4,c4])        ! { dg-error "Unequal character lengths .6/3. " }
  ALLOCATE (d,source=c1)             ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (e,source=c4)             ! { dg-error "Unequal character lengths .6/3. " }
  ALLOCATE (b,source=reshape([c1],[1,1])) ! { dg-error "Unequal character lengths .8/2. " }
  ALLOCATE (b,source=reshape([c2],[1,1]))
  ALLOCATE (f,source=reshape([c3],[1,1]))
  ALLOCATE (f,source=reshape([c4],[1,1])) ! { dg-error "Unequal character lengths .6/3. " }
contains
  subroutine foo (s)
    character(*), intent(in) :: s
    character(len=8), allocatable :: f(:), g
    ALLOCATE (f(3), source=s)
    ALLOCATE (d,    source=s)
    ALLOCATE (f(3), mold=s)
    ALLOCATE (d,    mold=s)
  end
end
