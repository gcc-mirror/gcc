! { dg-do compile }
! { dg-options "-Wdate-time" }
print *, __TIMESTAMP__  ! { dg-warning "might prevent reproduce builds" }
print *, __TIME__  ! { dg-warning "might prevent reproduce builds" }
print *, __DATE__  ! { dg-warning "might prevent reproduce builds" }
end
