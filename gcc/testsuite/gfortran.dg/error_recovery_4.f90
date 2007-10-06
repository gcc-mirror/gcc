! { dg-do compile }
! PR33609 ICE on arithmetic overflow
! Before patch, this segfaulted.
print *, real(huge(1.0_8),4) ! { dg-error "Arithmetic overflow" }
end
