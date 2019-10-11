#define AUTOVEC_FORTRAN(OP) subroutine f (r, x, y); \
  real(kind=kind (1.0d0)) :: r(1000000), x(1000000), y(1000000); \
  integer :: i; \
  do i = 1, 1000000; \
    r(i) = OP (x(i), y(i)); \
  end do; \
end
