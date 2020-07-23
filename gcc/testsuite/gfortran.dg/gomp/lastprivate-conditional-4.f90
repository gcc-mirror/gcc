module m
integer x, w
end module m

subroutine foo
  use m
  interface
    logical function bar(i)
      integer i
    end function
  end interface
  integer y, i, z
  logical tmp
  y = 5
  !$omp teams num_teams(1) firstprivate (x) shared (y) shared (w)
    !$omp parallel do firstprivate (x, y, z, w) lastprivate (conditional: x, y, z, w)
    do i = 1, 64
      if (bar (i)) then
        x = i;
        y = i + 1;
        z = i + 2;
        w = i + 3;
      end if
      tmp = bar (y);
      tmp = bar (z);
    end do
  !$omp end teams
end
