! { dg-do run }
! { dg-options "-std=f2008 " }
!

integer :: p, r, rdx

! Compile-time version

if (selected_real_kind(radix=2) /= 4) call should_not_fail()
if (selected_real_kind(radix=4) /= -5) call should_not_fail()
if (selected_real_kind(precision(0.0),range(0.0),radix(0.0)) /= kind(0.0)) &
  call should_not_fail()
if (selected_real_kind(precision(0.0d0),range(0.0d0),radix(0.0d0)) /= kind(0.0d0)) &
  call should_not_fail()

! Run-time version

rdx = 2
if (selected_real_kind(radix=rdx) /= 4) STOP 1
rdx = 4
if (selected_real_kind(radix=rdx) /= -5) STOP 2

rdx = radix(0.0)
p = precision(0.0)
r = range(0.0)
if (selected_real_kind(p,r,rdx) /= kind(0.0)) STOP 3

rdx = radix(0.0d0)
p = precision(0.0d0)
r = range(0.0d0)
if (selected_real_kind(p,r,rdx) /= kind(0.0d0)) STOP 4
end
