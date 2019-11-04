! { dg-do compile }
! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81509
!
program foo
logical :: a = .false.
integer :: i = 42
integer(8) :: k
k = iand(z'aaaa', z'1234')    ! { dg-error "cannot both be BOZ literal" }
k = and(z'aaaa', z'1234')     ! { dg-error "cannot both be BOZ literal" }
k = and(1, z'1234')
k = and(i, z'1234')
k = ieor(z'ade',i)
k = ior(i,z'1111')
k = ior(i,k)                  ! { dg-error "different kind type parameters" }
k = and(i,k)                  ! { dg-error "must be the same type" }
k = and(a,z'1234')            ! { dg-error "must be INTEGER" }
end program foo

