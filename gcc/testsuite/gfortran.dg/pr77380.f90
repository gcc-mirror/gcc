! { dg-do compile }
! { dg-options "-fcoarray=lib -O2" }
program p
   integer :: z(2)[*] = 1
   z(:)[1] = z(:)[*]       ! { dg-error "must be a scalar at" }
end
