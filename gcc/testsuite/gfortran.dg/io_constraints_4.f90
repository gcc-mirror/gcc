! { dg-do compile }
! PR33268 [patch,fortran] read ('(f3.3)'), a rejected due to the extra (...)

write(*,('(a)')) 'Hello'
write (*,'(f8.3)'), 3.14  ! { dg-warning "Comma before i/o item list" }
print ('(a)'), "valid"
read ('(f3.3)'), a 
read (*, '(f3.3)'), a     ! { dg-warning "Comma before i/o item list" }
write ('(a)'), "invalid"  ! { dg-error "Invalid form of WRITE statement" }
end
