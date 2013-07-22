! { dg-do compile }
!
! PR fortran/57894
!
! Contributed by Vittorio Zecca
!
print *, max(a2=2,a65=45,a2=5)         ! { dg-error "has already appeared in the current argument list" }
print *, min(a1=2.0,a65=45.0,a2=5.0e0) ! OK
print *, max(a2=2,a65=45,a3=5)         ! { dg-error "Missing 'a1' argument to the max intrinsic" }
print *, min(a1=2.0,a65=45.0,a3=5.0e0) ! { dg-error "Missing 'a2' argument to the min intrinsic" }
print *, min1(2.0,a1=45.0,a2=5.0e0) ! { dg-error "Duplicate argument 'a1'" }

print *, max0(a1=2,a65a=45,a2=5)  ! { dg-error "Unknown argument 'a65a'" }
print *, amax0(a1=2,as65=45,a2=5) ! { dg-error "Unknown argument 'as65'" }
print *, max1(a1=2,a2=45,5)       ! { dg-error "Missing keyword name in actual argument list" }
print *, amax1(a1=2,a3=45,a4=5)   ! { dg-error "Missing 'a2' argument" }
print *, dmax1(a1=2,a2=45,a4z=5)  ! { dg-error "Unknown argument 'a4z'" }

print *, min0(a1=2,a65a=45,a2=5)  ! { dg-error "Unknown argument 'a65a'" }
print *, amin0(a1=2,as65=45,a2=5) ! { dg-error "Unknown argument 'as65'" }
print *, min1(a1=2,a2=45,5)       ! { dg-error "Missing keyword name in actual argument list" }
print *, amin1(a1=2,a3=45,a4=5)   ! { dg-error "Missing 'a2' argument" }
print *, dmin1(a1=2,a2=45,a4z=5)  ! { dg-error "Unknown argument 'a4z'" }
end
