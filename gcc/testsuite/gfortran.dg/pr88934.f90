! { dg-do compile }
! { dg-options "-O -ftree-vectorize" }
! { dg-additional-options "-mvsx" { target powerpc*-*-* } }
integer, parameter :: a=3
  integer , dimension(a,a) :: b
  logical, dimension(a,a) :: c
  do i=0,1
     b = ltoi(c)
     do j=0,if
        if (anymatmul(b) /= 0) then
        end if
     end do
  end do
contains
  elemental function ltoi(d)
    logical, intent(in) :: d
    if (d) then
       ltoi = 1
    else
       ltoi = 0
    end if
  end
end
