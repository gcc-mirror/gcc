! { dg-do run }
!
! PR fortran/45424
! PR fortran/48820
!
! Run-time checks for IS_CONTIGUOUS

implicit none
integer, pointer :: a(:), b(:,:)
integer :: i, j, k, s

allocate(a(5), b(10,10))

s = 1
if (.true. .neqv. is_contiguous (a(::s))) stop 1
s = 2
if (.false. .neqv. is_contiguous (a(::s))) stop 2
i=5; j=7
if (.true. .neqv. is_contiguous (b(1:i*2,1:j))) stop 3
if (.false. .neqv. is_contiguous (b(1:i,1:j))) stop 4
i=5; j=5; s=1
if (.false. .neqv. is_contiguous (b(i:5:s,i:j*2))) stop 5

! The following test zero-sized arrays. For the standard, they
! are regarded as noncontiguous. However, gfortran in line with
! other compilers only checks for the strides and thus prints
! .true. or .false. depending on this setting.

s = 4
if (.false. .neqv. is_contiguous (a(2:1:s))) stop 6
s = 1
if (.true. .neqv. is_contiguous (a(2:1:s))) stop 7
end
