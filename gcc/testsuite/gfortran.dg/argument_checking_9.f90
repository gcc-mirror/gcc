! { dg-do compile }
! { dg-options "-fmax-errors=40" }
! PR33162 INTRINSIC functions as ACTUAL argument
! Prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program double_specs

real(kind=4) :: rr, x, y
real(kind=8) :: dr, dx, dy

x = .5
y = .7
dx = .5d0
dy = .5d0

r = dabs(x) ! { dg-error "must be double precision" }
r = dacos(x) ! { dg-error "must be double precision" }
r = dacosh(x) ! { dg-error "must be double precision" }
r = dasin(x) ! { dg-error "must be double precision" }
r = dasinh(x) ! { dg-error "must be double precision" }
r = datan(x) ! { dg-error "must be double precision" }
r = datanh(x) ! { dg-error "must be double precision" }
r = datan2(y, dx) ! { dg-error "must be double precision" }
r = datan2(dy, x) ! { dg-error "must be double precision" }
r = dbesj0(x) ! { dg-error "must be double precision" }
r = dbesj1(x) ! { dg-error "must be double precision" }
r = dbesy0(x) ! { dg-error "must be double precision" }
r = dbesy1(x) ! { dg-error "must be double precision" }
r = dcos(x) ! { dg-error "must be double precision" }
r = dcosh(x) ! { dg-error "must be double precision" }
r = ddim(x, dy) ! { dg-error "must be double precision" }
r = ddim(dx, y) ! { dg-error "must be double precision" }
r = derf(x) ! { dg-error "must be double precision" }
r = derfc(x) ! { dg-error "must be double precision" }
r = dexp(x) ! { dg-error "must be double precision" }
r = dgamma(x) ! { dg-error "must be double precision" }
r = dlgama(x) ! { dg-error "must be double precision" }
r = dlog(x) ! { dg-error "must be double precision" }
r = dlog10(x) ! { dg-error "must be double precision" }
r = dmod(x, dy) ! { dg-error "must be double precision" }
r = dmod(dx, y) ! { dg-error "must be double precision" }
r = dsign(x, dy) ! { dg-error "must be double precision" }
r = dsign(dx, y) ! { dg-error "must be double precision" }
r = dsin(x) ! { dg-error "must be double precision" }
r = dsinh(x) ! { dg-error "must be double precision" }
r = dsqrt(x) ! { dg-error "must be double precision" }
r = dtan(x) ! { dg-error "must be double precision" }
r = dtanh(x) ! { dg-error "must be double precision" }
dr = dprod(dx,y) ! { dg-error "must be default real" }
dr = dprod(x,dy) ! { dg-error "must be default real" }
dr = dprod(x,y)

end program double_specs