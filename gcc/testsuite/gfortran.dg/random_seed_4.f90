! { dg-do compile }
! PR fortran/95037
! This led to a segfault or a confusing error message.  Original
! test case by Bill Long.

subroutine my_random_seed_v (size, put, get)
integer, optional :: size
integer, optional :: put(1)
integer, optional :: get(1)
call random_seed (size, get=get) ! { dg-error "too small" }
call random_seed (size, put=put) ! { dg-error "too small" }
call random_seed (size, get=get, put=put) ! { dg-error "too small" }
end subroutine my_random_seed_v

