! { dg-do compile }
! Tests the fix for PR24168, in which line   would return
! Error: Incompatible ranks 2 and 1 in assignment at (1)
! This came about because the simplification of the binary
! operation, in the first actual argument of spread, was not
! returning the rank of the result.  Thus the error could
! be generated with any operator and other intrinsics than
! cshift.
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
 integer, parameter :: nx=2, ny=2
 real, dimension(nx, ny) :: f
 f = spread(2 * cshift((/ 1, 2 /), nx/2), 2, ny)
end

