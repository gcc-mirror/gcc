! { dg-do run }
! { dg-options "-fcheck=all" }
!
! PR fortran/71194
!
! Contributed by T Kondic
!
program ice
implicit none
integer, parameter :: pa=10, pb=20
complex, target :: a(pa*pb)
real, pointer:: ptr(:,:) =>null()
integer :: i, j, cnt
logical :: negative

  do i = 1, size(a)
    a(i) = cmplx(i,-i)
  end do

  ! Was ICEing before with bounds checks
  ptr(1:pa*2,1:pb) => conv2real(a)

  negative = .false.
  cnt = 1
  do i = 1, ubound(ptr,dim=2)
    do j = 1, ubound(ptr,dim=1)
      if (negative) then
        if (-cnt /= ptr(j, i)) call abort()
        cnt = cnt + 1
        negative = .false.
      else
        if (cnt /= ptr(j, i)) call abort()
        negative = .true.
      end if
    end do 
  end do

contains
  function conv2real(carr)
    use, intrinsic :: iso_c_binding
    ! returns real pointer to a complex array
    complex, contiguous, intent(inout), target  :: carr(:)
    real,contiguous,pointer :: conv2real(:)
    call c_f_pointer(c_loc(carr),conv2real,[size(carr)*2])
  end function conv2real
end program
