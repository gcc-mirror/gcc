! { dg-options "-fdec-math" }
! { dg-do compile }
!
! Ensure extra math intrinsics formerly offered by -fdec-math
! are still available with -fdec-math.
!

print *, sind(0.0)
print *, cosd(0.0)
print *, tand(0.0)
print *, cotan(1.0)
print *, cotand(90.0)

end
