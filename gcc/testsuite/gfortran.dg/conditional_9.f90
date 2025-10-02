! { dg-do compile }
! { dg-options "-std=f2023" }
implicit none
integer :: i, j
do concurrent (i=(j > 1 ? 0 : 1) : 5) local(j) ! { dg-error "must not appear in LOCAL locality-spec at" }
end do
do concurrent (i=(.true. ? j : 1) : 5) local(j) ! { dg-error "must not appear in LOCAL locality-spec at" }
end do
do concurrent (i=(.false. ? 1 : j) : 5) local(j) ! { dg-error "must not appear in LOCAL locality-spec at" }
end do
end
