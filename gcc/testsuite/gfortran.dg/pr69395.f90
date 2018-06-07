! { dg-do compile }
! { dg-options "-fcoarray=single" }
program p
real, dimension(1,2,1,2,1,2,1,2), codimension[1,2,1,2,1,2,*] :: y
real, dimension(1,2,1,2,1,2,1,2), codimension[1,2,1,2,1,2,1,*] :: z  ! { dg-error "allowed dimensions" }
end
