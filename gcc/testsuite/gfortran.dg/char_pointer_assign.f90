! { dg-do run }
! { dg-options "-std=legacy" }
!
program char_pointer_assign
! Test character pointer assignments, required
! to fix PR18890 and PR21297
! Provided by Paul Thomas pault@gcc.gnu.org
  implicit none
  character*4, target        :: t1
  character*4, target        :: t2(4) =(/"lmno","lmno","lmno","lmno"/)
  character*4                :: const
  character*4, pointer       :: c1, c3
  character*4, pointer       :: c2(:), c4(:)
  allocate (c3, c4(4))
! Scalars first.
  c3 = "lmno"          ! pointer = constant
  t1 = c3              ! target = pointer
  c1 => t1             ! pointer =>target
  c1(2:3) = "nm"
  c3 = c1              ! pointer = pointer
  c3(1:1) = "o"
  c3(4:4) = "l"
  c1 => c3             ! pointer => pointer
  if (t1 /= "lnmo") STOP 1
  if (c1 /= "onml") STOP 2

! Now arrays.
  c4 = "lmno"          ! pointer = constant
  t2 = c4              ! target = pointer
  c2 => t2             ! pointer =>target
  const = c2(1)
  const(2:3) ="nm"     ! c2(:)(2:3) = "nm" is still broken
  c2 = const
  c4 = c2              ! pointer = pointer
  const = c4(1)
  const(1:1) ="o"      ! c4(:)(1:1) = "o" is still broken
  const(4:4) ="l"      ! c4(:)(4:4) = "l" is still broken
  c4 = const
  c2 => c4             ! pointer => pointer
  if (any (t2 /= "lnmo")) STOP 3
  if (any (c2 /= "onml")) STOP 4
  deallocate (c3, c4)
end program char_pointer_assign

