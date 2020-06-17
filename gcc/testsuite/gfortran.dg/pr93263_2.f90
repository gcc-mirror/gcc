! { dg-do run }
!
! Test contributed by Tobias Burnus  <burnus@gcc.gnu.org>

  integer :: cnt
  cnt = 0
  call sub()
  if (cnt /= 5) stop 1
contains
  recursive subroutine sub()
    save
    logical :: first = .true.
    integer :: i
    cnt = cnt + 1
    if (first) then
      first = .false.
      i = 1
    end if
    print *, "Hello", i
    i = i + 1
    if (i <= 5) call sub()
  end subroutine
end

