! { dg-do compile }
! PR fortran/20858
! If we have "x = null(i)", then "null()" acquires the type, kind type,
! and rank of i and these need to match those of x.
program null_1
   integer, parameter :: sp = kind(1.e0), dp = kind(1.d0)
   integer,  pointer :: i => null()
   real(sp), pointer :: x => null()
   real(dp), pointer :: y => null()
   real(sp), pointer :: z(:) => null()
   x => null(i)     ! { dg-error "types in pointer assignment" }
   x => null(y)     ! { dg-error "types in pointer assignment" }
   z => null(i)     ! { dg-error "types in pointer assignment" }
   z => null(y)     ! { dg-error "types in pointer assignment" }
   x => null(z)     ! { dg-error "ranks in pointer assignment" }
   z => null(x)     ! { dg-error "ranks in pointer assignment" }
   z => null(z)
   nullify(i, x, y, z)
end program null_1
