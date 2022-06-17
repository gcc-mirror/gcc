! { dg-do compile }
! { dg-additional-options -fcoarray=single }
program p
   integer, allocatable :: z[:,:]
   integer :: i
   allocate (z[1:,*]) ! { dg-error "Bad coarray specification in ALLOCATE statement" }
   allocate (z[:2,*]) ! { dg-error "Bad coarray specification in ALLOCATE statement" }
   allocate (z[2:1,*]) ! { dg-error "Upper cobound is less than lower cobound" }
   allocate (z[:0,*]) ! { dg-error "Bad coarray specification in ALLOCATE statement" }
   allocate (z[0,*]) ! { dg-error "Upper cobound is less than lower cobound" }
   allocate (z[1,*]) ! This is OK
   allocate (z[1:1,*]) ! This is OK
   allocate (z[i:i,*]) ! This is OK
   allocate (z[i:i-1,*]) ! { dg-error "Upper cobound is less than lower cobound" }
end
