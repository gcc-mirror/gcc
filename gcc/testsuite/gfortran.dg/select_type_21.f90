! { dg-do compile }
! PR fortran/45848
! PR fortran/47204
!
select type (a) ! { dg-error "Selector shall be polymorphic" }
end select
end
